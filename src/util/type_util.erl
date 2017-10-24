-module(type_util).

%% API
-export([to_list/1,
         to_atom/1,
         to_binary/1,
         to_integer/1
         ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
to_list(undefined) ->
    "";
to_list(List) when is_list(List) ->
    List;
to_list(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Bin -> unicode:characters_to_list(Bin);
        _ -> binary_to_list(Bin)
    end;
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Number) when is_integer(Number) ->
    integer_to_list(Number);
to_list(Number) when is_float(Number) ->
    float_to_list(Number);
to_list(_) ->
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
to_atom(Atom) when is_atom(Atom) ->
    Atom;
to_atom(List) when is_list(List) ->
    list_to_atom(List);
to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(to_list(Bin));
to_atom(_) ->
    throw(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts to list
%%
%% @end
%%--------------------------------------------------------------------
to_binary(Bin) when is_binary(Bin)->
    Bin;
to_binary(List) when is_list(List) ->
    % case unicode:characters_to_binary(List) of
    %     {error,_,_} -> list_to_binary(List);
    %     B -> case unicode:characters_to_list(B,utf8) of
    %              List -> B;
    %              _ -> list_to_binary(List)
    %          end
    % end;
    list_to_binary(List);

to_binary(List) when is_list(List) ->
    case unicode:characters_to_binary(List) of
        {error,_,_} -> list_to_binary(List);
        B -> case unicode:characters_to_list(B,utf8) of
                 List -> B;
                 _ -> list_to_binary(List)
             end
    end;

to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_binary(Integer) when is_integer(Integer)->
    erlang:integer_to_binary(Integer);
to_binary(_) ->
    throw(badarg).
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
to_integer(Int) when is_integer(Int) ->
    Int;
to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(to_list(Bin));
to_integer(_) ->
    throw(badarg).
    

