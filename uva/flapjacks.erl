%% A semi-literate-programming Erlang solution for the **Stacks of Flapjacks** 
%% problem from [streamtech's](http://www.streamtech.nl/problemset/120.html) 
%% problem set by [Brendan Hay](http://www.github.com/brendanhay).

%% ### Brief
%% #### Background
%% Stacks and Queues are often considered the bread and butter of data structures 
%% and find use in architecture, parsing, operating systems, and discrete event simulation. 
%% Stacks are also important in the theory of formal languages.
%% This problem involves both butter and sustenance in the form of pancakes 
%% rather than bread in addition to a finicky server who flips pancakes 
%% according to a unique, but complete set of rules.
%%
%% #### Problem
%% Given a stack of pancakes, you are to write a program that indicates how the stack can 
%% be sorted so that the largest pancake is on the bottom and the smallest pancake is on the top.
%% The size of a pancake is given by the pancake's diameter. 
%% All pancakes in a stack have different diameters.
%% Sorting a stack is done by a sequence of pancake flips. 
%% A `flip` consists of inserting a spatula between two pancakes in a stack and flipping 
%% (reversing) the pancakes on the spatula (reversing the sub-stack).
%%  A flip is specified by giving the position of the pancake on the bottom of the 
%% sub-stack to be flipped (relative to the whole stack). The pancake on the bottom
%%  of the whole stack has position 1 and the pancake on the top of 
%% a stack of n pancakes has position n.
%% A stack is specified by giving the diameter of each pancake 
%% in the stack in the order in which the pancakes appear.
%%
%% #### Input
%% The input consists of a sequence of stacks of pancakes. Each stack will consist of between 
%% 1 and 30 pancakes and each pancake will have an integer diameter between 1 and 100. 
%% The input is terminated by end-of-file. Each stack is given as a single line of input 
%% with the top pancake on a stack appearing first on a line, the bottom pancake appearing 
%% last, and all pancakes separated by a space.
%%
%%     1 2 3 4 5
%%     5 4 3 2 1
%%     5 1 2 3 4
%%
%% #### Output
%% For each stack of pancakes, the output should echo the original stack on one line, 
%% followed by some sequence of flips that results in the stack of pancakes being 
%% sorted so that the largest diameter pancake is on the bottom and the smallest on top. 
%% For each stack the sequence of flips should be terminated by a 0 
%% (indicating no more flips necessary). Once a stack is sorted, no more flips should be made.
%% 
%%     1 2 3 4 5
%%     0
%%     5 4 3 2 1
%%     1 0
%%     5 1 2 3 4
%%     1 2 0

%% ### Running
%% Erlang is required, sources and binaries for various environments 
%% can be downloaded from [the official site](http://erlang.org/download.html).
%%
%% Assuming your prompt is in the same location as 
%% the problem file `./120/flapjacks.erl`, you can compile it:
%%
%%     $ erl
%%     1> c(flapjacks).
%%
%% You should see the following, indicating successful compilation:
%%
%%     {ok,flapjacks} 
%%
%% The problem can then be run:
%%
%%     2> flapjacks:start().
%%
%% And finally, it will prompt for an input string according to the problem brief (ie. `5 1 2 3 4`):
%%
%%     Pankcakes!:
%%

%% ### Module Definition

%% `sort/1` is exported for debugging but be warned it doesn't perform any sort 
%% of input sanitisation. `start/0` is exported as the top-level entry point.
-module(flapjacks).
-export([sort/1, start/0]).

%% ### Exported Functions

%% Start the pancake flipper by reading numeric space delimited input from 
%% the term: `5 1 2 3 4`
%% The first two lines of output are according to the brief,
%% with the following two lines describing the final state
%% of the stack and specific positions that were flipped.
start() ->
    Stack = read(),
    {Sorted, Ops} = sort(Stack),
    print(Stack, "Input"),
    print(count(Ops), "Output"),
    print(Sorted, "Sorted"),
    print(Ops, "Operations").

%% Sorts the stack of pancakes using only the `flip` algorithm described in the
%% problem brief. Calls `sort/2` with the `stack` and the expected outcome which
%% is achieved via the `O(n log n)` merge-sort in `lists:sort/1`. It returns 
%% a `tuple` consisting of the sorted result and the positions of the pancakes 
%% that were flipped, which needs to be reversed to be in the correct order
%% as the operations are recorded via prepending to the head.
sort([])    -> {[], []};
sort(Stack) -> 
    {Sorted, Ops} = sort(Stack, lists:sort(Stack)),
    {Sorted, lists:reverse(Ops)}.

%% ### Internal Functions

%% Interpret a single line of input from stdin using `parse/1` and `clean/1` to 
%% sanitise the input and turn it into a list of valid integers.
read() -> parse(clean(io:get_line("Pankcakes!: "))).

%% Strip leading/trailing spaces and newlines from both ends of the input.
clean(Input) -> string:strip(string:strip(Input), both, $\n).

%% Convert each token to a string accumulating the *valid* *unique* results by
%% calling `validate/2` and `unique/1` respectively.
%% Returns the first 30 diameters from the reversed accumulator once there are
%% no more tokens to process.
parse(Input)      -> parse(string:tokens(Input, " "), []).

parse([H|T], Acc) -> parse(T, validate(string:to_integer(H), Acc));
parse([], Acc)    -> uniques(lists:sublist(lists:reverse(Acc), 30)).

%% Filter the result of `string:to_integer/1` by accumulating valid integers 
%% that are inside the range specified in the `guard` and ignoring all others.
validate({Int, _}, Acc) when is_integer(Int), Int >= 1, Int =< 100 -> [Int|Acc];
validate(_, Acc)                                                   -> Acc.

%% Create a duplicate free list via a highly inefficient but cute list comprehension.
uniques([])    -> [];
uniques([H|T]) -> [H | [X || X <- uniques(T), X /= H]].

%% Format the number of operations (with a special case for `0`) according to
%% question brief output. For example the operations: `4 5 3 0` becomes output: `1 2 3 0`.
count(Ops)              -> count(Ops, 1, []).

count([], _, Acc)       -> lists:reverse(Acc);
count([0|T], Iter, Acc) -> count(T, Iter, [0|Acc]);
count([_|T], Iter, Acc) -> count(T, Iter + 1, [Iter|Acc]).

%% Print the resulting pancake stack in a nicer 'non-erlang-termerised' format
%% with a heading column by using the term formatter `~p` for each integer in the stack.
print(Stack, Heading) ->
    io:fwrite("~-12.s", [Heading ++ ":"]),
    print(Stack).

print(Stack) ->
    io:fwrite(string:copies("~p ", length(Stack)) ++ "~n", Stack).

%% Naive sort by always looking for the largest pancake first,
%% and since it takes max of two flips for any additional sorting -
%% best, average, and worst cases are all roughly equivalent. 
%% Returns the sorted stack and the list of operations (in reverse order) that
%% were used to transform the input stack -> output. `0` will be at the head
%% of the list indicating that the stack was sorted according to the order
%% specification that was passed in.
sort([], _)              -> {[], [0]};
sort(Stack, Stack)       -> {Stack, [0]};
sort(Stack=[H|_], Order) ->
    Length = length(Stack),
    Last = lists:last(Stack),
    Ops = case lists:max(Stack) of
      	      H    -> [Length];
      	      Last -> [];
	      Max  -> [position(Max, Stack), Length]
	  end,
    {NewStack, CompletedOps} = flip(Ops, Stack),
    sort(Length - 1, NewStack, Order, CompletedOps).

sort(_, [], _, Ops)           -> {[], Ops};
sort(1, Stack, _, Ops)        -> {Stack, Ops};
sort(_, Stack, Stack, Ops)    -> {Stack, [0|Ops]};
sort(Iter, Stack, Order, Ops) -> 
    {NewStack, CompletedOps} = flip([position(Iter, Stack), Iter], Stack, Ops),
    sort(Iter - 1, NewStack, Order, CompletedOps).  

%% Find the position of a flapjack in the stack by recursively pattern matching
%% on each element of the list from left to right.
position(_, [])     -> -1; 
position(N, [N|_])  -> 1; 
position(N, [_|T])  -> position(N, T) + 1. 

%% Flip the stack according to the `flip` algorithm described in the problem brief.
%% Trying to flip the first pankcake is considered a noop and isn't recorded 
%% as an operation. The list is split using `lists:split/2` and then the top part 
%% (the sub-stack on the spatula) is reversed using `lists:reverse/1`, the position 
%% is then either accumulated or return as the operation performed.
flip(Input, Stack)         -> flip(Input, Stack, []).

flip([], Stack, Ops)       -> {Stack, Ops};
flip(1, Stack, Ops)        -> {Stack, Ops};
flip([H|T], Stack, Ops)    -> 
    {NewStack, NewOps} = flip(H, Stack, Ops),
    flip(T, NewStack, NewOps);
flip(Position, Stack, Ops) ->
    {Top, Bottom} = lists:split(Position, Stack),
    {lists:reverse(Top) ++ Bottom, [Position|Ops]}.

