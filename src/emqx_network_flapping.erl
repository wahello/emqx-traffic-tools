%%%-------------------------------------------------------------------
%%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(emqx_network_flapping).

%%============================================================================================
%%    note:
%%         What emqx_networking_flapping should do:
%%     　　1. What is the Action on (Port or IP or Client) State
%%         2. When the (Port or Ip or Client) is back to normal
%%         3. Whether to ban the (Port or ip or Client) forever
%%    steps:
%%         1. check State transitions in a specified rate in a specified times.
%%         2. count the times of State transitions
%%         3. assign weight for State transition
%%         4. calculate percent of state change with weight
%%         5. compare the state change percent with high flap threshold and low flap threshold
%%    Parameters: (get from user) :
%%         1. Check Times, Time Interval
%%         2. High flap threshold and low flap threshold.
%%         3. current State and Function to check state.
%%    Return(three states):
%%         1. flapping_start
%%         2. flapping_stop
%%         3. do_nothing
%%=============================================================================================

-export([
         check_network_flapping/7
        ]).

-spec(check_network_flapping(pos_integer(), pos_integer(), float(), float(),
                             Fun::fun(), term(), atom()) -> atom()).
check_network_flapping(CheckTimes, TimeInterval, HighFlapTreshold,
                       LowFlapThreshold, CheckState, State,
                       OldFlapState) ->
    Weights = check_state_transition(CheckTimes,TimeInterval, CheckTimes, CheckState, State, 0),
    case Weights/CheckTimes of
        StateTransRate when StateTransRate >= HighFlapTreshold,
                            OldFlapState =:= flapping_stop ->
            flapping_start;
        StateTransRate when StateTransRate =< LowFlapThreshold,
                            OldFlapState =:= flapping_start ->
            flapping_stop;
        _ ->
            do_nothing
    end.

-spec(weight_transition(pos_integer(), pos_integer()) -> float()).
weight_transition(CheckTimes, TotalTimes) ->
    %%  calculate current weight
    %%  The scope is from 0.8 to 1.2
    %%  the weight transition in every time = (1.2 - 0.8) / TotalTimes
    0.8 + trunc(0.4/TotalTimes) * CheckTimes.

-spec(check_state_transition(non_neg_integer(), pos_integer(), pos_integer(), Fun::fun(), term(), pos_integer()) -> pos_integer()).
check_state_transition(0, _TimeInterval, _Total, _CheckState, _State, Weight) ->
    Weight;
check_state_transition(Count, TimeInterval, Total, CheckState, State, Weight)
  when Count > 0->
    timer:sleep(TimeInterval),
    NewState = CheckState(),
    case State =:= NewState of
        false ->
            NewWeight = Weight + weight_transition(Total - Count + 1, Total),
            check_state_transition(Count - 1, TimeInterval, Total, CheckState, NewState, NewWeight);
        true ->
            check_state_transition(Count - 1, TimeInterval, Total, CheckState, State, Weight)
    end.
