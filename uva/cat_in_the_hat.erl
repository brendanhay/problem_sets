%% A semi-literate-programming Python solution for the **Cat in the Hat**
%% problem from [streamtech's](http://www.streamtech.nl/problemset/107.html) 
%% problem set by [Brendan Hay](http://www.github.com/brendanhay). 

%% ### Brief 
%% #### Background 
%% (An homage to Theodore Seuss Geisel)
%% The Cat in the Hat is a nasty creature,
%% But the striped hat he is wearing has a rather nifty feature.
%% With one flick of his wrist he pops his top off.
%% Do you know what's inside that Cat's hat?
%% A bunch of small cats, each with its own striped hat.
%% Each little cat does the same as line three,
%% All except the littlest ones, who just say "Why me?"
%% Because the littlest cats have to clean all the grime,
%% And they're tired of doing it time after time!
%% 
%% #### Problem
%% A clever cat walks into a messy room which he needs to clean. Instead of doing 
%% the work alone, it decides to have its helper cats do the work. It keeps its 
%% (smaller) helper cats inside its hat. Each helper cat also has helper cats in 
%% its own hat, and so on. Eventually, the cats reach a smallest size. These smallest 
%% cats have no additional cats in their hats. These unfortunate smallest cats have
%% to do the cleaning. The number of cats inside each (non-smallest) cat's hat 
%% is a constant, N. The height of these cats-in-a-hat is `1/N+1` times the height 
%% of the cat whose hat they are in. The smallest cats are of height one; 
%% these are the cats that get the work done. All heights are positive integers.
%% Given the height of the initial cat and the number of worker cats (of height one), 
%% find the number of cats that are not doing any work (cats of height greater than one) 
%% and also determine the sum of all the cats' heights (the height of a stack of all 
%% cats standing one on top of another).
%% 
%% #### Input
%% The input consists of a sequence of cat-in-hat specifications. Each specification 
%% is a single line consisting of two positive integers, separated by white space. 
%% The first integer is the height of the initial cat, and the second integer is 
%% the number of worker cats. A pair of 0's on a line indicates the end of input.
%% 
%%     216 125
%%     5764801 1679616
%%     0 0
%% 
%% #### Output
%% For each input line (cat-in-hat specification), print the number of cats that are 
%% not working, followed by a space, followed by the height of the stack of cats. 
%% There should be one output line for each input line other than the `0 0`
%% that terminates input.
%% 
%%     31 671
%%     335923 30275911
%% 

%% ### Running
%% Erlang is required, sources and binaries for various environments 
%% can be downloaded from [the official site](http://erlang.org/download.html).
%%
%% Assuming your prompt is in the same location as the problem file `cat_in_the_hat.erl`, 
%% and the input file `cat_in_the_hat.txt`, you can compile it:
%%
%%     $ erl
%%     1> c(cat_in_the_hat).
%%
%% You should see the following, indicating successful compilation:
%%
%%     {ok,cat_in_the_hat} 
%%
%% The problem can then be run:
%%
%%     2> cat_in_the_hat:start().
%%

%% ### Module Definition

%% `start/0` is exported as the top-level entry point.
-module(cat_in_the_hat).
-export([start/0]).

%% Open the file and pass the `IO` device to `read/1` applying the subsequent steps to the
%% previous results until `print/1` is reached.
start() ->
    {ok, IO} = file:open("cat_in_the_hat.txt", [read]),
    print(total(count(read(IO)))).

%% Print the resulting list of unemployed cats and total height of the stack of cats
%% according to the problem brief.	
print([]) -> ok;
print([{Unemployed, Height}|T]) ->
    io:fwrite("~p ~p\n", lists:map(fun trunc/1, [Unemployed, Height])),
    print(T).    

%% Sugar for `read/2` to specify a default accumulator.
read(IO)      -> read(IO, []).  
 
%% Read a two digits on a line from `IO` device and parse the result using `parse/2`,
%% which in turn calls this to request another line, accumulating the parsed results
%% and returning from `parse/2` when `eof` or the problem brief's `0 0` stop is reached.
read(IO, Acc) -> parse(IO, io:fread(IO, "", "~d ~d"), Acc).

%% Pattern match and accumulate the two expected digits from a formatted line from
%% `IO` device.
%%
%% No need to reverse the accumulator on return as `count/1` and `count/2` don't reverse either
%% and prepend to their accumulators.
parse(_, eof, Acc)                      -> Acc;
parse(_, {ok, [0, 0]}, Acc)             -> Acc;
parse(IO, {ok, [Height, Workers]}, Acc) -> read(IO, [{Height, Workers}|Acc]);
parse(IO, _, Acc)                       -> read(IO, Acc).

%% Sugar for `count/2` to specify a default accumulator.
count(Cats)                       -> count(Cats, []).  

%% Accumulate the measurements for a list of parsed inputs.  
count([], Acc)                    -> Acc;
count([{Height, Workers}|T], Acc) -> count(T, [measure(Height, Workers)|Acc]).

%% Handle `Height: 1, Workers: 1` seperately as `measure/3` doesn't work for this case.
measure(Height, 1)       -> K = math:log(Height) / math:log(2), {Height, 1, K, K};
measure(Height, Workers) -> measure(Height, Workers, 2).

%% Measure the number of cat's inside any cat with a height greater than 1's hat, and
%% the number of workers.
%%
%% `K` is the number of cats inside each (non-smallest) cat's hat.
%% `T` is the number of cats with different heights.
%%
%% Since I'm dealing with floats, I can't use `lists:seq` and other range operators, 
%% hence the use of index `N`. 
measure(Height, Workers, N) ->
    K = math:log(Workers) / math:log(N),
    T = math:pow(N + 1, K) - Height,
    if 
	T < 0.0000001 ->
	    {Height, N, K, (math:pow(N, K) - 1) / (N - 1)};
	true ->
	    measure(Height, Workers, N + 1)
    end.  

%% Sugar for `count/2` to specify a default accumulator.
total(Measurements) -> 
    total(Measurements, []).   

%% Accumulate the sums of a list of counted/measured inputs, 
%% `Unemployed` is the total number of cat's not working and 
%% `N / (N + 1)` is the height of a cat in a hat which is used as the initial increment
%% to determine the total height of a stack of cats using `sum/5`.
total([], Acc) -> 
    lists:reverse(Acc);
total([{Height, N, K, Unemployed}|T], Acc) ->
    total(T, [{Unemployed, sum(N / (N + 1), Height, Height, 1, K + 0.1)}|Acc]).   

%% Sum the total height of the stack of cats where `Max` is the number of cats
%% inside all non-working cat's hats.
sum(Step, Height, Total, N, Max) when N =< Max  ->
    sum(Step, Step * Height, Step * Height + Total,  N + 1, Max);
sum(_, _, Total, _, _) ->
    Total.
