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

%% Export everything during development.
-module(gondwanaland_telecom).
-compile(export_all).

%% ### Exported Functions

%% ### Internal Functions
