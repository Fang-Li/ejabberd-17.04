-module(aa_tuple).
-compile(export_all).

-spec combine(tuple()) -> list().

combine(A) when is_tuple(A) ->
  List = tuple_to_list(A),
  combine(List);
  
combine(A) when is_list(A) ->
  case A of
    [] -> 
      [];
    _ ->
      combine(odd, A, _Acc = [])
  end.

combine(odd, A, Acc) ->
  [Key|Tail] =  A,
  case Tail of
    [] ->
      L = [{Key,error}|Acc],
      lists:reverse(L);
    _ ->
      combine(even, Key, Tail, Acc)
  end.
  
combine(even, Key, A, Acc) ->
  [Value|Tail] = A,
  case Tail of 
    [] ->
      L = [{Key,Value} | Acc],
      lists:reverse(L);
    _ ->
      combine(odd, Tail, [{Key,Value} | Acc])
  end.
      
