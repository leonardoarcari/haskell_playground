-module (exercise_3).
-export ([activate/2, leafy/1, branchy/3,
          test/0]).

% Consider a binary tree stored with tuples, e.g.
%     {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}}.

% Define an activate function, which takes a binary tree and a binary
% function f and creates a network of actors having the same
% structure of the given tree.
% Actors corresponding to leaves run a function called leafy, 
% that must be defined, which answer to the message {ask, P} by
% sending to process P the pair {self(), V},
% where V is the value stored in the leaf, then terminate.
% Actors for the branches run a function called branchy, that must be also
% defined: if they receive an {ask, P} request like that of
% leaves, they ask both their sons; when they receive the answers,
% they call f on the obtained values, then send the result V to P as
% {self(), V} and terminate.
% E.g. running the following code:
%     test() ->
%         T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
%         A1 = activate(T1, fun min/2),
%         A1 ! {ask, self()},
%         receive
%             {A1, V} -> V
%         end.
% should return 1.

waitBranches(Left, Right, nil, nil) ->
    receive
        {Left, V} -> waitBranches(Left, Right, V, nil);
        {Right, V} -> waitBranches(Left, Right, nil, V)
    end;

waitBranches(Left, Right, LVal, RVal) ->
    receive
        {Left, V} -> {V, RVal};
        {Right, V} -> {V, LVal}
    end.

branchy(F, Left, Right) ->
    receive
        {ask, P} ->
            Left ! {ask, self()},
            Right ! {ask, self()},
            {LVal, RVal} = waitBranches(Left, Right, nil, nil),
            P ! {self(), F(LVal, RVal)}
    end.

leafy(V) ->
    receive
        {ask, P} -> P ! {self(), V}
    end.

activate({leaf, V}, _) -> spawn(?MODULE, leafy, [V]);
activate({branch, Left, Right}, BinaryFunction) ->
    LPid = activate(Left, BinaryFunction),
    RPid = activate(Right, BinaryFunction),
    spawn(?MODULE, branchy, [BinaryFunction, LPid, RPid]).

% Driver
test() ->
    T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
    A1 = activate(T1, fun min/2),
    A1 ! {ask, self()},
    receive
        {A1, V} -> V
    end.