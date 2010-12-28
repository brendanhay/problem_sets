%% A semi-literate-programming Erlang solution for the **Gondwanaland Telecom** 
%% problem from [streamtech's](http://www.streamtech.nl/problemset/145.html) 
%% problem set by [Brendan Hay](http://www.github.com/brendanhay).

%% ### Brief
%% #### Background
%% Gondwanaland Telecom makes charges for calls according to distance and time of day.
%% The basis of the charging is contained in the following schedule, where the 
%% charging step is related to the distance:
%%
%% <table>
%%   <tr>
%%     <th>Charging Step (distance)</th>
%%     <th>Day Rate 8am to 6pm</th>
%%     <th>Evening Rate 6pm to 10pm</th>
%%     <th>Night Rate 10pm to 8am</th>
%%   </tr>
%%   <tr>
%%     <td>A</td>
%%     <td>0.10</td>
%%     <td>0.06</td>
%%     <td>0.02</td>
%%   </tr>
%%   <tr>
%%     <td>B</td>
%%     <td>0.25</td>
%%     <td>0.15</td>
%%     <td>0.05</td>
%%   </tr>
%%   <tr>
%%     <td>C</td>
%%     <td>0.53</td>
%%     <td>0.33</td>
%%     <td>0.13</td> 
%%   </tr>
%%   <tr>
%%     <td>D</td>
%%     <td>0.87</td>
%%     <td>0.47</td>
%%     <td>0.17</td>
%%   </tr>
%%   <tr>
%%     <td>E</td>
%%     <td>1.44</td>
%%     <td>0.80</td>
%%     <td>0.30</td>
%%   </tr>
%% </table>
%% <br/>
%%
%% All charges are in dollars per minute of the call. Calls which straddle a rate
%% boundary are charged according to the time spent in each section. Thus a call
%% starting at 5:58 pm and terminating at 6:04 pm will be charged for 2 minutes 
%% at the day rate and for 4 minutes at the evening rate. Calls less than a minute
%%  are not recorded and no call may last more than 24 hours.
%% Write a program that reads call details and calculates the corresponding charges.
%%
%% #### Input
%% Input lines will consist of the charging step (upper case letter 'A'..'E'), 
%% the number called (a string of 7 digits and a hyphen in the approved format) 
%% and the start and end times of the call, all separated by exactly one blank. 
%% Times are recorded as hours and minutes in the 24 hour clock, separated by one 
%% blank and with two digits for each number. Input will be terminated by a line
%% consisting of a single #.
%%
%%     A 183-5724 17 58 18 04
%%     #
%%
%% #### Output
%% Output will consist of the called number, the time in minutes the call spent 
%% in each of the charge categories, the charging step and the total cost in the
%% format shown below.
%%
%% <table>
%%   <tr>
%%     <th>Number</th>
%%     <th>Day</th>
%%     <th>Evening</th>
%%     <th>Night</th>
%%     <th>Step</th>
%%     <th>Cost</th>
%%   </tr>
%%   <tr>
%%     <td>183-5724</td>
%%     <td>2</td>
%%     <td>4</td>
%%     <td>0</td>
%%     <td>A</td>
%%     <td>0.44</td>
%%   </tr>
%% </table>
%%

%% ### Running
%% Erlang is required, sources and binaries for various environments 
%% can be downloaded from [the official site](http://erlang.org/download.html).
%%
%% Assuming your prompt is in the same location as 
%% the problem file `gondwanaland_telecom.erl`, you can compile it:
%%
%%     $ erl
%%     1> c(gondwanaland_telecom).
%%
%% You should see the following, indicating successful compilation:
%%
%%     {ok,gondwanaland_telecom} 
%%
%% The problem can then be run:
%%
%%     2> gondwanaland_telecom:start().
%%
%% And finally, it will prompt for an input string according to the problem brief (ie. `A 183-5724 17 58 18 04`):
%%
%%     Call:
%%

%% ### Module Definition

%% `start/0` is exported as the top-level entry point.
-module(gondwanaland_telecom).
-export([start/0]).

%% Record specifying how the input line is deconstructed.
-record(input, {step, number, start_hour, start_min, end_hour, end_min}).

%% ### Exported Functions
-spec start() -> 'ok'.
start() ->
    Input = read(),
    Durations = durations(Input),
    Cost = cost(Input#input.step, Durations),    
    print(Input, Durations, Cost).

%% ### Internal Functions

%% Interpret a single line of input from stdin using `parse/1` and `clean/1` to 
%% sanitise the input and turn it into a valid input record.
-spec read() -> #input{}.    
read() -> parse(clean(io:get_line("Call: "))).

%% Strip leading/trailing spaces and newlines from both ends of the input.
-spec clean(iolist()) -> string().
clean(Input) -> string:strip(string:strip(Input), both, $\n).

%% Convert a string or iolist into a binary, recursing over each `time`
%% after desconstructing the intial `step` and `number`.
-spec parse(binary() | iolist()) -> #input{}.
parse(<<Step:8, " ", Number:64/bitstring, T/binary>>) -> 
    parse(T, [Step, binary_to_list(Number)]);
parse(String) -> 
    parse(list_to_binary(String)).

-spec parse(binary(), list()) -> #input{}.
parse(<<>>, Acc) -> 
    list_to_tuple([input|Acc]);
parse(<<" ", Time:16/bitstring, T/binary>>, Acc) ->
    parse(T, Acc ++ [list_to_integer(binary_to_list(Time))]).

%% Print the ouput according to the problem brief.
-spec print(#input{}, [tuple()], number()) -> 'ok'.
print(#input{step=Step, number=Number}, Durations, Cost) ->
    Data = [Number, [D || {_, D} <- Durations], Step, Cost],
    io:fwrite("~s ~p ~c ~-5.2f~n", Data).

%% Calculate the total calling time for each time range, passing the potentially
%% modified start hour/minutes from the previous function ala function 
%% composition like so:
%%
%%     evening(day(night()))
%%
%% Returning a list of `{range, duration}` tuples.
-spec durations(#input{}) -> [{atom(), number()}].    
durations({input, _, _, SH, SM, EH, EM}) ->
    durations([night, day, evening], {SH, SM, EH, EM}, []).

-spec durations([atom()], tuple(), [{atom(), number()}]) -> [{atom(), number()}].
durations([], _, Acc)        -> Acc;
durations([H|T], Times, Acc) -> 
    {Duration, ModTimes} = calculate(H, Times),
    durations(T, ModTimes, [{H, Duration}|Acc]).

%% Calculate the duration of the call in the a time range, returning the 
%% duration and a potentially updated set of start hour/minutes.
%% 
%% The time ranges are calculated as follows:
%%
%%     Day: 8:00 - 17:59
%%     Evening: 18:00 - 21:59
%%     Night: 22:00 - 7:59
%%
-spec calculate(atom(), tuple()) -> {number(), tuple()}.    
calculate(night, {SH, SM, EH, EM}) when SH < 8, EH >= 8 ->
    {(SH - 8) * 60 - SM, {8, 0, EH, EM}};
calculate(night, {SH, SM, EH, EM}=Times) when SH < 8 ->
    {(EH - SH) * 60 + EM - SM, Times};
calculate(day, {SH, SM, EH, EM}) when EH >= 18, SH < 18 ->
    {(18 - SH) * 60 - SM, {18, 0, EH, EM}};
calculate(day, {SH, SM, EH, EM}=Times) when EH >= 8, SH < 18 ->
    {(EH - SH) * 60 + EM - SM, Times};
calculate(evening, {SH, SM, EH, EM}) when EH >= 22, SH < 22 ->
    {(22 - SH) * 60 - SM, {22, 0, EH, EM}};
calculate(evening, {SH, SM, EH, EM}=Times) when EH >= 18, SH < 22 ->
    {(EH - SH) * 60 + EM - SM, Times};
calculate(_, Times) ->
    {0, Times}.

%% Calculate the cost for a specific step given a list of `{range, duration}` tuples.
-spec cost(char(), [{atom(), number()}]) -> number().
cost(Step, Durations)                    -> cost(Step, Durations, 0).    

-spec cost(char(), [{atom(),number()}], number()) -> number().
cost(_, [], Total)                       -> Total;
cost(Step, [{Range, Duration}|T], Total) -> cost(Step, T, Duration * rate(Step, Range) + Total).

%% Get the calling rate keyed by charging step for each of respective time ranges.
-spec rate(char(), day | evening | night) -> float().
rate(Step, Range) -> lists:nth(abs($A - Step) + 1, rates(Range)).

-spec rates(day | evening | night) -> [float()].    
rates(day)     -> [0.10, 0.25, 0.53, 0.87, 1.44];
rates(evening) -> [0.06, 0.15, 0.33, 0.47, 0.80];
rates(night)   -> [0.02, 0.05, 0.13, 0.17, 0.30].
