%% A semi-literate-programming Erlang solution for
%% [Project Euler](http://projecteuler.net/index.php?section=problems&id=89) **Problem 89** 
%% by [Brendan Hay](http://www.github.com/brendanhay).

%% ### Brief
%% The rules for writing Roman numerals allow for many ways of writing each number (see FAQ: Roman Numerals). However, there is always a "best" way of writing a particular number.
%% For example, the following represent all of the legitimate ways of writing the number sixteen:
%%
%%      IIIIIIIIIIIIIIII
%%      VIIIIIIIIIII
%%      VVIIIIII
%%      XIIIIII
%%      VVVI
%%      XVI
%% 
%% The last example being considered the most efficient, as it uses the least number of numerals.
%% The 11K text file, roman.txt, contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals; that is, they are arranged in descending units and obey the subtractive pair rule (see FAQ for the definitive rules for this problem).
%% Find the number of characters saved by writing each of these in their minimal form.
%% *Note:* You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.

%% ### Running
%% Erlang is required, sources and binaries for various environments 
%% can be downloaded from [the official site](http://erlang.org/download.html).
%%
%% Assuming your prompt is in the same location as 
%% the problem file `flapjacks.erl`, you can compile it:
%%
%%     $ erl
%%     1> c(problem_89).
%% 
%% You should see the following, indicating successful compilation:
%%
%%     {ok,problem_89} 
%%
%% The problem can then be run:
%%
%%     2> problem_89:compute().
%%
%% Which will read the problem file `roman.txt` from the curent working directory
%% and print the computed result.

%% ### Module Definition

%% `compute/0` is exported as the top level entry point to calculate the resulting character
%% savings by reducing the lines in `roman.txt` to their most efficient form.
-module(problem_89).
-export([compute/0]).

%% A list of tuples containing the match and optimum replacement value which is used
%% to reduce numerals to their most efficient forms.
-define(NUMERALS, [{<<"DCCCC">>, <<"CM">>}, 
		   {<<"LXXXX">>, <<"XC">>}, 
		   {<<"VIIII">>, <<"IX">>},
		   {<<"IIII">>, <<"IV">>}, 
		   {<<"XXXX">>, <<"XL">>}, 
		   {<<"CCCC">>, <<"CD">>}]).

%% ### Exported Functions

%% Accrues the original and modified numerals from `roman.txt` into two binaries, streaming
%% lines and modifying them as read. The output is the difference between the original and 
%% modified version of the file contents, which is the number of characters saved by 
%% performing the reduction/replacements.
compute() ->
    {Original, Modified} = map_lines("roman.txt", fun replace/1),
    byte_size(Original) - byte_size(Modified).   

%% ### Internal Functions

%% Open `Path` in `read` and `binary` mode for a small performance gain when performing 
%% stripping and match replacement on individual lines.  
map_lines(Path, Fun) ->
    {ok, Device} = file:open(Path, [read, binary]),
    map_line(Device, Fun, [], []).

%% For each line until `eof` is received accumulate the original line, and the result of
%% applying `Fun`. Both results are cleaned using `clean/1` to remove the trailing `\n`.
map_line(Device, Fun, Original, Modified) ->
    case io:get_line(Device, "") of
        eof  -> 
	    file:close(Device),
	    {list_to_binary(Original), list_to_binary(Modified)};
        Line -> 
	    map_line(Device, Fun, [clean(Line)|Original], [clean(Fun(Line))|Modified])
    end.

%% Applied to a `binary` it performs a replacement using a `{Match, Replace}` tuple 
%% from the `?NUMERALS` macro defined above. `binary:replace/4` is defined in native code.
%% Every `{Match, Replace}` tuple is applied to `Bin` from left to right to get the result.
replace(Bin) -> 
    Fold = fun({Match, Replace}, B) -> binary:replace(B, Match, Replace, []) end,
    lists:foldl(Fold, Bin, ?NUMERALS).

%% Removes the last character of a `binary`.
clean(Bin) -> 
    binary:part(Bin, {0, byte_size(Bin) - 1}).
