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
-module(emqx_token_bucket).

-export([
         init_ets/0,
         init_token_bucket/3,
         init_token_bucket_ets/1,
         info_token_bucket_ets/0,
         check_token_bucket/2
        ]).

-record(token_bucket, {
                       burst_size    :: non_neg_integer(), % max number of tokens required
                       limit_tokens  :: pos_integer(),     % number of limit tokens
                       interval      :: pos_integer(),     % specified interval time milliseconds
                       remain_tokens :: non_neg_integer(), % number of remained tokens in token bucket
                       last_time     :: pos_integer()      % milliseconds
                      }).

-type(token_bucket() :: #token_bucket{}).

%%------------------------------------------------------------------
%% Note:
%%
%%   Four parameters :
%%       * enable.rate.limit
%%       * rate.limit.average
%%       * rate.limit.time.unit
%%       * rate.limit.burst.size
%%       b ->  bucket size
%%       R -> transmission rate
%%       r -> token added rate or limit rate
%%       burst_time = b / (R-r)
%%       burst_size = burst_time * R
%%       b = burst_size / R * (R - r)
%%       r =  tokens / TimeUnit or limit rate
%%
%%------------------------------------------------------------------

%% @doc create ets table
init_ets() ->
    ets:new(buckets, [named_table, public, set,
                      {read_concurrency, true},
                      {write_concurrency, true}
                     ]).


%% @doc create token bucket in ets
-spec(init_token_bucket_ets(token_bucket()) -> atom()).
init_token_bucket_ets(TokenBucket) ->
    ets:insert(buckets, TokenBucket).

%% @doc lookup token bucket in ets
-spec(info_token_bucket_ets() -> list()).
info_token_bucket_ets() ->
    [TokenBucket | _] = ets:lookup(buckets, token_bucket),
    TokenBucket.

%% @doc create token bucket
-spec(init_token_bucket(non_neg_integer(), pos_integer(), pos_integer()) -> token_bucket()).
init_token_bucket(BurstSize, LimitTokens, Interval) when BurstSize > LimitTokens ->
    #token_bucket{ burst_size = BurstSize,
                   limit_tokens = LimitTokens,
                   interval = Interval,
                   remain_tokens = LimitTokens,
                   last_time = now_ms()}.


-spec(check_token_bucket(non_neg_integer(), atom()|token_bucket()) -> {non_neg_integer(), token_bucket()}).
check_token_bucket(Dataflow, with_ets) ->
    TokenBucket = info_token_bucket_ets(),
    {token_bucket, BurstSize, LimitTokens, Interval, RemainTokens, LastTime} = TokenBucket,
    {Pause, NewTokenBucket} =  check_token_bucket(Dataflow, #token_bucket{
                                                                   burst_size = BurstSize,
                                                                   remain_tokens = RemainTokens,
                                                                   interval = Interval,
                                                                   limit_tokens = LimitTokens ,
                                                                   last_time = LastTime
                                                                  }),
    case ets:insert(buckets, NewTokenBucket) of
        true ->
            Pause;
        false ->
            check_token_bucket(Dataflow,with_ets)
    end;
check_token_bucket(Dataflow, TokenBucket = #token_bucket{burst_size = BurstSize,
                                                         remain_tokens = RemainTokens,
                                                         interval = Interval,
                                                         limit_tokens = LimitTokens ,
                                                         last_time = LastTime}) ->
    Tokens = erlang:min(BurstSize, RemainTokens
                        + trunc(LimitTokens
                                * (now_ms() - LastTime)
                                / Interval)),
    case Tokens >= Dataflow of
        true  -> %% Tokens available
            {0, TokenBucket#token_bucket{remain_tokens = Tokens - Dataflow,
                                         last_time = now_ms()}};
        false -> %% Tokens not enough
            {Interval, TokenBucket#token_bucket{remain_tokens = 0,
                                                last_time = now_ms()
                                                + Interval}}
    end.


%% [TokenBucket| _] = ets:lookup(buckets, token_bucket:)

%% @doc inflow data.

%%-----------------------------------------
%%  internal function
%%-----------------------------------------
now_ms() ->
    now_ms(os:timestamp()).

now_ms({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs/1000).

