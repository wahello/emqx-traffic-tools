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
         create_flapping_records/0,
         drop_flapping_records/0,
         init_flapping_record/5,
         add_flapping_record/1,
         update_flapping_record/1,
         info_flapping_record/1,
         delete_flapping_record/1,
         check_network_flapping/3,
         check_network_flapping_record/3
        ]).

-record(flapping_record, {
                          name                :: atom(),          % the name of flapping record
                          flapping_state      :: atom(),          % flapping state
                          check_times         :: pos_integer(),   % check times specified
                          time_interval       :: pos_integer(),   % time interval between each check(milliseconds)
                          high_flap_treshold  :: float(),         % high flapping treshold specified
                          low_flap_treshold   :: float()          % low  flapping treshold specified
                         }).

-type(flapping_record() :: #flapping_record{}).

-spec create_flapping_records() -> atom().
create_flapping_records() ->
    ets:new(flapping_records, [named_table, public, set,
                               {read_concurrency, true},
                               {write_concurrency, true},
                               {keypos, 2}]).

drop_flapping_records() ->
    ets:delete(flapping_records).

-spec add_flapping_record(flapping_record()) -> atom().
add_flapping_record(FlappingRecord) ->
    ets:insert(flapping_records, FlappingRecord).

-spec update_flapping_record(flapping_record()) -> atom().
update_flapping_record(FlappingRecord) ->
    ets:insert(flapping_records, FlappingRecord).

-spec delete_flapping_record(atom()) -> boolean().
delete_flapping_record(Name) ->
    ets:delete(flapping_records, Name).

info_flapping_record(Name) ->
    [FlappingRecord | _] = ets:lookup(flapping_records, Name),
    FlappingRecord.

-spec init_flapping_record(atom(), pos_integer(), pos_integer(),float(),
                          float()) -> flapping_record().
init_flapping_record(Name, CheckTimes, TimeInterval, HighFlapTreshold,
                    LowFlapTreshold) ->
    #flapping_record{
       name = Name,
       flapping_state = flapping_stop,
       check_times = CheckTimes,
       time_interval = TimeInterval,
       high_flap_treshold = HighFlapTreshold,
       low_flap_treshold = LowFlapTreshold
      }.

check_network_flapping_record(CheckStateFun, StateChecked, Name) ->
    FlappingRecord = info_flapping_record(Name),
    NextFlappingRecord = check_network_flapping(CheckStateFun, StateChecked, FlappingRecord),
    update_flapping_record(NextFlappingRecord).

-spec check_network_flapping(Fun::fun(), term(), flapping_record()) -> {atom(), flapping_record()}.
check_network_flapping(CheckStateFun, StateChecked,
                       FlappingRecord = #flapping_record{
                                           flapping_state = FlappingState,
                                           check_times = CheckTimes,
                                           time_interval = TimeInterval,
                                           high_flap_treshold = HighFlapTreshold,
                                           low_flap_treshold = LowFlapTreshold
                                          }) ->
    Weights = check_state_transition(CheckTimes,TimeInterval, CheckTimes, CheckStateFun, StateChecked, 0),
    case Weights/CheckTimes of
        NewFlappingState when NewFlappingState >= HighFlapTreshold,
                              FlappingState =:= flapping_stop ->
            FlappingRecord#flapping_record{
              flapping_state = flapping_start
             };
        NewFlappingState when NewFlappingState =< LowFlapTreshold,
                              FlappingState =:= flapping_start ->
            FlappingRecord#flapping_record{
              flapping_state = flapping_stop
             };
        _ ->
            FlappingRecord
    end.


-spec weight_transition(pos_integer(), pos_integer()) -> float().
weight_transition(CheckTimes, TotalTimes) ->
    0.8 + 0.4/TotalTimes * CheckTimes.

-spec check_state_transition(non_neg_integer(), pos_integer(), pos_integer(), Fun::fun(), term(), pos_integer()) -> pos_integer().
check_state_transition(0, _TimeInterval, _Total, _CheckState, _State, Weight) ->
    Weight;
check_state_transition(Count, TimeInterval, Total, CheckState, State, Weight)
  when Count > 0->
    timer:sleep(TimeInterval),
    NewState = CheckState(),
    case State =:= NewState of
        false ->
            NewWeight = Weight + weight_transition(Total - Count + 1, Total),
            io:format("Weight : ~p  ~p  ~p~n", [Weight, Total - Count + 1, Total]),
            check_state_transition(Count - 1, TimeInterval, Total, CheckState, NewState, NewWeight);
        true ->
            check_state_transition(Count - 1, TimeInterval, Total, CheckState, State, Weight)
    end.
