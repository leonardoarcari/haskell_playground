-module (exercise_3).
-export ([parfilter/2, testPredicate/2,
          test/0]).

% Define a parallel implementation of the classical functional
% higher order function filter.
testPredicate(Predicate, Value) ->
    receive
        {Pid, test} -> Pid ! {self(), Predicate(Value), Value}
    end.

joinWorkers([]) -> [];
joinWorkers([_|Tail]) ->
    receive
        {_, true, Value} -> [Value|joinWorkers(Tail)];
        {_, false, _} -> joinWorkers(Tail)
    end.

parfilter(Predicate, List) ->
    Workers = [spawn(?MODULE, testPredicate, [Predicate, X]) || X <- List],
    lists:foreach(fun(Pid) -> Pid ! {self(), test} end, Workers),
    joinWorkers(Workers).

% Driver
test() ->
    List = lists:seq(0, 10),
    Even = fun(X) -> X rem 2 == 0 end,
    parfilter(Even, List).