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

-module(emqx_network_flapping_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%-----------------------------------------
%%  general state machine for test
%%-----------------------------------------
-behaviour(gen_statem).

all() ->
    [test_check_flapping_with_ets,
     test_check_flapping_without_ets].

%% test flapping check with ets:
test_check_flapping_with_ets(_Config) ->
    {ok, _} = start_link(),
    emqx_network_flapping:create_flapping_records(),
    FlappingRecord = emqx_network_flapping:init_flapping_record(test, 30, 1, 45/100, 30/100),
    emqx_network_flapping:add_flapping_record(FlappingRecord),
    emqx_network_flapping:check_network_flapping_record(fun check_state_high_flap/0,
                                                        check_state_high_flap(),
                                                        test),
    _FlappingRecord1 = emqx_network_flapping:info_flapping_record(test),
    emqx_network_flapping:check_network_flapping_record(fun check_state_normal_flap/0,
                                                        check_state_normal_flap(),
                                                        test),
    _FlappingRecord2 = emqx_network_flapping:info_flapping_record(test),
    emqx_network_flapping:check_network_flapping_record(fun check_state_low_flap/0,
                                                        check_state_low_flap(),
                                                        test),
    _FlappingRecord3 = emqx_network_flapping:info_flapping_record(test),
    emqx_network_flapping:delete_flapping_record(test),
    emqx_network_flapping:drop_flapping_records(),
    stop().


%% test flapping check without ets
test_check_flapping_without_ets(_Config) ->
    {ok, _} = start_link(),
    FlappingRecord = emqx_network_flapping:init_flapping_record(test,30,1, 45/100, 30/100),
    %% {flapping_stop, FlappingState1} =
    FlappingRecord1 = emqx_network_flapping:check_network_flapping(
                        fun check_state_high_flap/0,
                        check_state_high_flap(),
                        FlappingRecord),
    io:format("Check_state_high_flap: ~p~n", [FlappingRecord1]),
    FlappingRecord2 = emqx_network_flapping:check_network_flapping(
                        fun check_state_normal_flap/0,
                        check_state_normal_flap(),
                        FlappingRecord1),
    io:format("Check_state_normal_flap: ~p~n", [FlappingRecord2]),
    FlappingRecord3 = emqx_network_flapping:check_network_flapping(
                        fun check_state_low_flap/0,
                        check_state_low_flap(),
                        FlappingRecord2),
    io:format("Check_state_low_flap: ~p~n", [FlappingRecord3]),
    stop().

%%%===================================================================
%%% general state machine
%%%===================================================================

-define(SERVER, ?MODULE).

check_state_low_flap() ->
    gen_statem:call(?SERVER, check_state_low_flap).

check_state_high_flap() ->
    gen_statem:call(?SERVER, check_state_high_flap).

check_state_normal_flap() ->
    gen_statem:call(?SERVER, check_state_normal_flap).

stop() ->
    gen_statem:stop(?SERVER).

-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
init([]) ->
    process_flag(trap_exit, true),
    Data = 0,
    {ok, state_a, Data}.

handle_event({call, From}, check_state_high_flap, State, Data) ->
    NewData = Data + 8,
    NewState = state_switch(State, NewData),
    {next_state, NewState, NewData, {reply, From , NewState}};
handle_event({call, From}, check_state_normal_flap, State, Data) ->
    NewData = Data + 4,
    NewState = state_switch(State, NewData),
    {next_state, NewState, NewData, {reply, From, NewState}};
handle_event({call, From}, check_state_low_flap, State, Data) ->
    NewData = Data + 1,
    NewState = state_switch(State, NewData),
    {next_state, NewState, NewData, {reply, From, NewState}}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    ok.

state_switch(State, Data) ->
    case Data rem 8 of
        Result when Result =:= 0,
                    State =:= state_a ->
            state_b;
        Result when Result =:= 0,
                    State =:= state_b ->
            state_a;
        _  ->
            State
    end.
