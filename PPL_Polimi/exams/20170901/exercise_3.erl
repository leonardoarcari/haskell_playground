-module (exercise_3).
-export ([parfind/2, find/3]).
% Define a parfind (parallel find) operation, which takes a list of lists L and
% a value x, and parallely looks for x in every list of L
% the idea is to launch one process for each list, searching for x. If x is
% found, parfind returns one of the lists containing x; otherwise, it
% returns false.
%
% E.g.
%     parfind([[1,2,3],[4,5,6],[4,5,9,10]], 4)
% could return either
%     [4,5,6] or [4,5,9,10];
% while
%     parfind([[1,2,3],[4,5,6],[4,5,9,10]], 7)
% is false.

find(Pid, List, X) ->
    case lists:member(X, List) of
        true -> Pid ! {self(), {ok, List}};
        false -> Pid ! {self(), not_found}
    end.

waitFind(NNotFound, NLists) when NNotFound =:= NLists -> not_found;
waitFind(NNotFound, NLists) ->
    receive
        {_, {ok, FoundList}} -> FoundList;
        {_, not_found} -> waitFind(NNotFound + 1, NLists)
    end.

parfind(LoL, X) ->
    [spawn(?MODULE, find, [self(), List, X]) || List <- LoL],
    case waitFind(0, length(LoL)) of
        not_found -> false;
        List -> List
    end.