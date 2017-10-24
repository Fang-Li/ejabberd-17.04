%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Дек. 2014 15:40
%%%-------------------------------------------------------------------

-module(aa_mc_auth).
-compile(export_all).

-include("include/mongo_protocol.hrl").
-include("include/ejabberd.hrl").
-include("include/logger.hrl").

-define(RANDOM_LENGTH, 24).
-define(AUTH_CMD(Login, Nonce, Password),
  {
    <<"authenticate">>, 1,
    <<"user">>, Login,
    <<"nonce">>, Nonce,
    <<"key">>, mc_utils:pw_key(Nonce, Login, Password)
  }).

-define(ALLOWED_CHARS, {65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
  90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
  116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57}).

-define(GS2_HEADER, <<"n,,">>).
% -record('query', {
%   collection ,%% :: colldb(),
%   tailablecursor = false :: boolean(),
%   slaveok = false :: boolean(),
%   sok_overriden = false :: boolean(),
%   nocursortimeout = false :: boolean(),
%   awaitdata = false :: boolean(),
%   skip = 0 :: mc_worker_api:skip(),
%   batchsize = 0 :: mc_worker_api:batchsize(),
%   selector :: mc_worker_api:selector(),
%   projector %% = #{} :: mc_worker_api:projector()
% }).

% -record('query', {
%   tailablecursor = false :: boolean(),
%   slaveok = false :: boolean(),
%   nocursortimeout = false :: boolean(),
%   awaitdata = false :: boolean(),
%   collection :: collection(),
%   skip = 0 :: skip(),
%   batchsize = 0 :: batchsize(),
%   selector :: selector(),
%   projector = [] :: projector()
% }).

%% ost level virture
aa_connect(Conf, Database) ->
  Options = [
    % {host,"10.250.72.79"},
    {host,"10.221.168.153"},
    {port,27017},
    {timeout,3000},
    {database,<<"optmsg">>},
    {login,<<"yueni">>},
    {password,<<"lt1803">>},
    {w_mode,unsafe},
    {r_mode,master}],
    {ok,Socket} = mc_auth:connect_to_database(Options),
    aa_auth(Socket, Conf, Database).

%% 1st level api
aa_auth(Socket, Conf, Database) ->
  NetModule = gen_tcp, %% or ssl
  Login = mc_utils:get_value(login, Conf),
  Password = mc_utils:get_value(password, Conf),
  auth(Socket, Login, Password, Database, Conf,NetModule).


%% 2nd level private 
auth(Socket, Login, Password, Database, Conf,NetModule) ->
  Version = get_version(Socket, Database, NetModule),
  do_auth(Version, Socket, Database, Login, Password, Conf,NetModule).

%% @private
% -spec do_auth(float(), port(), binary(), binary() | undefined, binary() | undefined, module()) -> boolean().
do_auth(_, _, _, Login, Pass, _Conf, _) when Login == undefined; Pass == undefined -> true; %do nothing
do_auth(Version, Socket, Database, Login, Password, _Conf, NetModule) when Version > 2.7 ->  %new authorisation
  scram_sha_1_auth(Socket, Database, Login, Password, NetModule);
do_auth(_, Socket, Database, Login, Password, Conf, NetModule) ->   %old authorisation
  mc_auth:auth(Socket, Conf, Database).


-spec scram_sha_1_auth(port(), binary(), binary(), binary(), module()) -> boolean().
scram_sha_1_auth(Socket, Database, Login, Password, SetOpts) ->
  try
    scram_first_step(Socket, Database, Login, Password, SetOpts)
  catch
    _:Reason ->
      Err = erlang:get_stacktrace(),
      ?ERROR_MSG("scram_first_step error ~n~p~n~p",[Reason,Err])
      % erlang:error(<<"Can't pass authentification">>)
  end.


scram_first_step(Socket, Database, Login, Password, SetOpts) ->
  RandomBString = random_binary(?RANDOM_LENGTH),
  FirstMessage = compose_first_message(Login, RandomBString),
  Message = base64:encode(<<?GS2_HEADER/binary, FirstMessage/binary>>),
  {true, Res} = sync_command(Socket, Database,
    {<<"saslStart">>, 1, <<"mechanism">>, <<"SCRAM-SHA-1">>, <<"autoAuthorize">>, 1, <<"payload">>, Message}, SetOpts),
  Res2 = aa_tuple:combine(Res),
  ConversationId = proplists:get_value(conversationId, Res2, {}),
  Payload = proplists:get_value(payload, Res2),
  scram_second_step(Socket, Database, Login, Password, Payload, ConversationId, RandomBString, FirstMessage, SetOpts).
  % ok.

%% @private
scram_second_step(Socket, Database, Login, Password, Payload, ConversationId, RandomBString, FirstMessage, SetOpts) ->
  Decoded = base64:decode(Payload),
  {Signature, ClientFinalMessage} = compose_second_message(Decoded, Login, Password, RandomBString, FirstMessage),
  {true, Res} = sync_command(Socket, Database, {<<"saslContinue">>, 1, <<"conversationId">>, ConversationId,
    <<"payload">>, base64:encode(ClientFinalMessage)}, SetOpts),
  scram_third_step(base64:encode(Signature), Res, ConversationId, Socket, Database, SetOpts).
  % ok.

%% @private
scram_third_step(ServerSignature, Response, ConversationId, Socket, Database, SetOpts) ->
  Response2 = aa_tuple:combine(Response),
  Payload = proplists:get_value(payload, Response2),
  Done = proplists:get_value(done, Response2, false),
  ParamList = parse_server_responce(base64:decode(Payload)),
  ServerSignature = mc_utils:get_value(<<"v">>, ParamList),
  scram_forth_step(Done, ConversationId, Socket, Database, SetOpts).
  % ok.

%% @private
scram_forth_step(true, _, _, _, _) -> true;
scram_forth_step(false, ConversationId, Socket, Database, SetOpts) ->
  {true, Res} = sync_command(Socket, Database, {<<"saslContinue">>, 1, <<"conversationId">>,
    ConversationId, <<"payload">>, <<>>}, SetOpts),
  Res2 = aa_tuple:combine(Res),
  true = proplists:get_value(done, Res2, false).
  % ok.


%% 3nd level private
get_version(Socket, Database, SetOpts) ->
  % {true, #{<<"version">> := Version}} = sync_command(Socket, Database, {<<"buildinfo">>, 1}, SetOpts),
  {true, Tuple} = sync_command(Socket, Database, {<<"buildinfo">>, 1}, SetOpts),
  Version = erlang:element(2,Tuple),
  {VFloat, _} = string:to_float(binary_to_list(Version)),
  VFloat.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come from mc_utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec random_binary(integer()) -> binary().
random_binary(Length) ->
  % rand:seed(exsplus, os:timestamp()),
  <<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
  random:seed(A,B,C),
  Chrs = ?ALLOWED_CHARS,
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  list_to_binary(lists:foldl(F, "", lists:seq(1, Length))).


%% Export for test purposes
compose_first_message(Login, RandomBString) ->
  UserName = <<<<"n=">>/binary, (encode_name(Login))/binary>>,
  Nonce = <<<<"r=">>/binary, RandomBString/binary>>,
  <<UserName/binary, <<",">>/binary, Nonce/binary>>.

-spec encode_name(binary()) -> binary().
encode_name(Name) ->
  Comma = re:replace(Name, <<"=">>, <<"=3D">>, [{return, binary}]),
  re:replace(Comma, <<",">>, <<"=2C">>, [{return, binary}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come from  mc_worker_api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Execute MongoDB command in this thread
% -spec sync_command(port(), binary(), mc_worker_api:selector(), module()) -> {boolean(), map()}.
sync_command(Socket, Database, Command, SetOpts) ->
  Doc = read_one_sync(Socket, Database, #'query'{
    collection = <<"$cmd">>,
    selector = Command
  }, SetOpts),
  mc_connection_man:process_reply(Doc, Command).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come from mc_action_man
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_one_sync(Socket, Database, Request, SetOpts) ->
  {0, Docs} = request_raw(Socket, Database, Request#'query'{batchsize = -1}, SetOpts),
  case Docs of
    % [] -> #{};
    [] -> [];
    [Doc | _] -> Doc
  end.

request_raw(Socket, Database, Request, NetModule) ->
  mc_connection_man:request_sync(Socket, Database, Request).
  % Timeout = mc_utils:get_timeout(),
  % ok = set_opts(Socket, NetModule, false),
  % {ok, _, _} = make_request(Socket, NetModule, Database, Request),
  % {ok, Packet} = NetModule:recv(Socket, 0, Timeout),
  % ok = set_opts(Socket, NetModule, true),
  % {Responses, _} = mc_worker_logic:decode_responses(Packet),
  % {_, Reply} = hd(Responses),
  % reply(Reply).

%% @private
% set_opts(Socket, ssl, Value) ->
%   ssl:setopts(Socket, [{active, Value}]);
% set_opts(Socket, gen_tcp, Value) ->
%   inet:setopts(Socket, [{active, Value}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come form mc_worker_logic 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -spec make_request(pid(), atom(), mc_worker_api:database(), mongo_protocol:message()) ->
  % {ok | {error, any()}, integer(), pos_integer()}.
% make_request(Socket, NetModule, Database, Request) ->
%   {Packet, Id} = encode_request(Database, Request),
%   {NetModule:send(Socket, Packet), byte_size(Packet), Id}.

% % -spec encode_request(mc_worker_api:database(), mongo_protocol:message()) -> {binary(), pos_integer()}.
% encode_request(Database, Request) ->
%   RequestId = mongo_id_server:request_id(),
%   Payload = mongo_protocol:put_message(Database, Request, RequestId),
%   {<<(byte_size(Payload) + 4):32/little, Payload/binary>>, RequestId}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come form mc_auth_logic 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Export for test purposes
compose_second_message(Payload, Login, Password, RandomBString, FirstMessage) ->
  ParamList = parse_server_responce(Payload),
  R = mc_utils:get_value(<<"r">>, ParamList),
  Nonce = <<<<"r=">>/binary, R/binary>>,
  {0, ?RANDOM_LENGTH} = binary:match(R, [RandomBString], []),
  S = mc_utils:get_value(<<"s">>, ParamList),
  I = binary_to_integer(mc_utils:get_value(<<"i">>, ParamList)),
  SaltedPassword = hi(pw_hash(Login, Password), base64:decode(S), I),
  ChannelBinding = <<<<"c=">>/binary, (base64:encode(?GS2_HEADER))/binary>>,
  ClientFinalMessageWithoutProof = <<ChannelBinding/binary, <<",">>/binary, Nonce/binary>>,
  AuthMessage = <<FirstMessage/binary, <<",">>/binary, Payload/binary, <<",">>/binary, ClientFinalMessageWithoutProof/binary>>,
  ServerSignature = generate_sig(SaltedPassword, AuthMessage),
  Proof = generate_proof(SaltedPassword, AuthMessage),
  {ServerSignature, <<ClientFinalMessageWithoutProof/binary, <<",">>/binary, Proof/binary>>}.

%% @private
parse_server_responce(Responce) ->
  ParamList = binary:split(Responce, <<",">>, [global]),
  lists:map(
    fun(Param) ->
      [K, V] = binary:split(Param, <<"=">>),
      {K, V}
    end, ParamList).

%% @private
hi(Password, Salt, Iterations) ->
  {ok, Key} = pbkdf2:pbkdf2(sha, Password, Salt, Iterations, 20),
  Key.

%% @private
generate_sig(SaltedPassword, AuthMessage) ->
  ServerKey = hmac(SaltedPassword, "Server Key"),
  hmac(ServerKey, AuthMessage).


%% @private
generate_proof(SaltedPassword, AuthMessage) ->
  ClientKey = hmac(SaltedPassword, <<"Client Key">>),
  StoredKey = crypto:hash(sha, ClientKey),
  Signature = hmac(StoredKey, AuthMessage),
  ClientProof = xorKeys(ClientKey, Signature, <<>>),
  <<<<"p=">>/binary, (base64:encode(ClientProof))/binary>>.

%% @private
xorKeys(<<>>, _, Res) -> Res;
xorKeys(<<FA, RestA/binary>>, <<FB, RestB/binary>>, Res) ->
  xorKeys(RestA, RestB, <<Res/binary, <<(FA bxor FB)>>/binary>>).



% 低版本
% process_reply(Doc, Command) ->
%   case bson:lookup(ok, Doc) of
%     {N} when N == 1 -> {true, bson:exclude([ok], Doc)};   %command succeed
%     {N} when N == 0 -> {false, bson:exclude([ok], Doc)};  %command failed
%     _Res -> erlang:error({bad_command, Doc}, [Command]) %unknown result
%   end.

% 高版本
% process_reply(Doc = #{<<"ok">> := N}, _) when is_number(N) ->   %command succeed | failed
%   {N == 1, maps:remove(<<"ok">>, Doc)};
% process_reply(Doc, Command) -> %unknown result
%   erlang:error({bad_command, Doc}, [Command]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% come from mc_utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

hmac(One, Two) -> crypto:hmac(sha, One, Two). 

pw_hash(Username, Password) ->
  bson:utf8(binary_to_hexstr(crypto:hash(md5, [Username, <<":mongo:">>, Password]))).

%% @private
binary_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

