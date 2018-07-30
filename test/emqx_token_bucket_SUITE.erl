%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_token_bucket_SUITE).


-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%-----------------------------------------
%%  Common Test
%%-----------------------------------------

all() ->
    [
     test_token_bucket_without_ets,
     test_token_bucket_with_ets
    ].

make_dataflow_without_ets(Datas, TokenBucket) ->
    timer:sleep(5),
    ConsumeData = consume_data(10),
    {Pause, NewTokenBucket} = emqx_token_bucket:check_token_bucket(ConsumeData, TokenBucket),
    timer:sleep(Pause),
    case Datas  of
        Datas when Datas >= ConsumeData ->
            make_dataflow_without_ets(Datas - ConsumeData, NewTokenBucket);
        Datas when Datas < ConsumeData, Datas > 0 ->
            make_dataflow_without_ets(Datas, NewTokenBucket);
        Datas when Datas =:= 0 ->
            ok
    end.

make_dataflow_with_ets(Datas) ->
    ets:new(datas, [named_table, public, set,
                    {read_concurrency, true},
                    {write_concurrency, true}]),
    ets:insert(datas, {remain_datas, Datas}).

make_processes_consume_ets(ProcNum, Pid) ->
    [spawn_link(fun() ->
                        consume_data_in_data_ets(Pid) end)
     || _ <- lists:seq(1, ProcNum)].

consume_data_in_data_ets(Pid) ->
    timer:sleep(10),
    ConsumeData = consume_data(10),
    Pause = emqx_token_bucket:check_token_bucket(ConsumeData, with_ets),
    timer:sleep(Pause),
    [DataRecord | _] = ets:lookup(datas, remain_datas),
    {remain_datas, RemainDatas} = DataRecord,
    case RemainDatas of
        RemainDatas when RemainDatas >= ConsumeData ->
            ets:insert(datas,{remain_datas, RemainDatas - ConsumeData}),
            consume_data_in_data_ets(Pid);
        RemainDatas when RemainDatas < ConsumeData, RemainDatas > 0 ->
            ets:insert(datas,{remain_datas, RemainDatas}),
            consume_data_in_data_ets(Pid);
        RemainDatas when RemainDatas =:= 0 ->
            Pid ! exit
    end.

test_token_bucket_with_ets(_Config) ->
    ProcessNum = 2,
    emqx_token_bucket:init_ets(),
    TokenBucket = emqx_token_bucket:init_token_bucket(100, 5, 5),
    emqx_token_bucket:init_token_bucket_ets(TokenBucket),
    make_dataflow_with_ets(200),
    make_processes_consume_ets(ProcessNum, self()),
    receive
        _exit ->
            ok
    end.

test_token_bucket_without_ets(_Config) ->
    TokenBucket = emqx_token_bucket:init_token_bucket(100, 5, 5),
    make_dataflow_without_ets(100, TokenBucket).


consume_data(ConsumeData) ->
    rand:uniform(ConsumeData).

%%-----------------------------------------
%%  Eunit Test
%%-----------------------------------------

token_bucket_test_()->
    TokenBucket = emqx_token_bucket:init_token_bucket(100, 5, 1000),
    [
     ?_assertMatch(buckets, emqx_token_bucket:init_ets()),
     ?_assertMatch(true, emqx_token_bucket:init_token_bucket_ets(TokenBucket)),
     ?_assertMatch({0, _},
                   emqx_token_bucket:check_token_bucket(5, TokenBucket)),
     ?_assertMatch({1000, _},
                   emqx_token_bucket:check_token_bucket(10, TokenBucket)),
     ?_assertMatch({token_bucket,_BurstSize, _LimitTokens, _Interval, _RemainTokens, _LastTime},
                   emqx_token_bucket:info_token_bucket_ets()),
     ?_assertEqual(0,
                   emqx_token_bucket:check_token_bucket(5, with_ets)),
     ?_assertEqual(1000,
                   emqx_token_bucket:check_token_bucket(10, with_ets)),
     ?_assertMatch(true, ets:delete(buckets))
    ].
