-module (exercise_3).
-export ([create_dlist/1, dlist_to_list/1, dlist_map/2]).

% We want to define a “dynamic list” data structure, where each element of the
% list is an actor storing a value. Such value can be of course read and set,
% and each get/set operation on the list can be performed in parallel.

% 1) Define create_dlist, which takes a number n and returns a dynamic list of
%    length n. You can assume that each element store the value 0 at start.
% 2) Define the function dlist_to_list, which takes a dynamic list and returns
%    a list of the contained values.
% 3) Define a map for dynamic list. Of course this operation has side effects,
%    since it changes the content of the list.

dlist_elm(Value) ->
    receive
        {Pid, get} ->
            Pid ! {self(), Value},
            dlist_elm(Value);
        {Pid, {set, NewValue}} ->
            Pid ! {self(), ok},
            dlist_elm(NewValue)
    end.

create_dlist(N) ->
    [spawn(fun() -> dlist_elm(0) end) || _ <- lists:seq(0, N)].

dlist_to_list([]) -> [];
dlist_to_list([Head|Tail]) ->
    Head ! {self(), get},
    receive
        {Head, Value} -> [Value | dlist_to_list(Tail)]
    end.

dlist_map(Fun, DL) ->
    lists:foreach(fun(Elm) ->
                    Elm ! {self(), get},
                    receive
                        {Elm, Value} ->
                            Elm ! {self(), {set, Fun(Value)}},
                            receive
                                {Elm, ok} -> ok
                                after 500 -> error_dlist_map
                            end
                    end
                  end
                  , DL).
