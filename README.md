# Emqx Traffic Tools

## Introduction

This suit of tools aims to provide tools to control network traffic. It is still under development and token bucket algorithm has been finished now.

The chart below shows how token bucket algorithm works:

![rate-limit](./images/01-rate-limit.png)



## Token Bucket Algorithm

* **emqx_token_bucket** - Simple (token bucket) implementation which by returning a time interval for user to control network data flow.

The Token Bucket Algorithm implemented in Emqx Traffic Tools not only supports network traffic in single process, but also supports to manage a pool of processes by using ets to record tokens.

**Example usage** (without ets):
``` erlang
%% BurstSize = 100(reqs or packets or messages)
%% LimitTokesn = 5 (Tokens to be consumed and added in token bucket)
%% Interval = 5 (specified interval time milliseconds)

TokenBucket = emqx_token_bucket:init_tokent_bucket(100, 5, 5),

{Pause, NewTokenBucket} = emqx_token_bucket:check_token_bucket(ConsumeData, TokenBucket).
%% 
```
The function `check_token_bucket` returns pause time which user should wait and a new token bucket state for next use. `ConsumeData` means the numbers of messages(packets or requests) per unit have to be transferred, it is not equal to the numbers of messages(packets or requests) which would be actually consumed or the tokens actually be taken.

If your application has a pool of processes and you want to limit the rate of the whole pool' messages or requests or packets. You can try emqx_token_bucket with ets.

**Example usage** (with ets):

``` erlang
emqx_token_bucket:init_ets(),
TokenBucket = emqx_token_bucket:init_token_bucket(100, 5, 5),
emqx_token_bucket:init_token_bucket_ets(TokenBucket),

Pause = emqx_token_bucket:check_token_bucket(ConsumeData, with_ets),
```
You can also get the token bucket state in ets.
```
TokenBucket = emqx_token_bucket:info_token_bucket().
```
