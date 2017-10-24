-module(mnesia_util).
-compile(export_all).



traversal_mnesia(Table) ->
  First = mnesia:dirty_first(Table),
  traversal_mnesia(Table, First,[]).
traversal_mnesia(_Table,Current,Acc) when Current == '$end_of_table' ->
  Acc;
traversal_mnesia(Table, Current,Acc) ->
  Acc1 = [Current|Acc],
  Next = mnesia:dirty_next(Table,Current),
  traversal_mnesia(Table,Next,Acc1).
