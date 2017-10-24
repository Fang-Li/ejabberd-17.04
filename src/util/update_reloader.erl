%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(update_reloader).

-include_lib("kernel/include/file.hrl").

-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1]).
%-export([test_msg/1,test_msg_time/3]).

%%%用这个文件时一定要小心：：：：：：任何有关环境变量 启动参数 及内存数据在重新加载时都会没有了 
%%%>>>>>>>>>不光要看本文件  就连头文件也要非常留意才行

%% External API

%% @spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

%% @spec all_changed() -> [atom()]
%% @doc Return a list of beam modules that have changed.
all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
            false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
             reload(Module);
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             io:format("Error reading ~s's file info: ~p~n",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
    io:format("Reloading ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            io:format(" ok.~n"),
            case erlang:function_exported(Module, test, 0) of
                true ->
                    io:format(" - Calling ~p:test() ...", [Module]),
                    case catch Module:test() of
                        ok ->
                            io:format(" ok.~n"),
                            reload;
                        Reason ->
                            io:format(" fail: ~p.~n", [Reason]),
                            reload_but_test_failed
                    end;
                false ->
                    reload
            end;
        {error, Reason} ->
            io:format(" fail: ~p.~n", [Reason]),
            error
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
