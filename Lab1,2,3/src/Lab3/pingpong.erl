%%%-------------------------------------------------------------------
%%% @author paranoidppl
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2025 13:28
%%%-------------------------------------------------------------------
-module(pingpong).
-author("paranoidppl").

%% API
-export([play/1, stop/0, start/0]).

start() ->
    register(ping, spawn(fun() -> ping_loop(0) end)),
    register(pong, spawn(fun() -> pong_loop() end)).


ping_loop(N) ->
    receive
        stop ->
            ok;
        0 ->
            io:format("ping got N=0"),
            ping_loop(N+1);
        D ->
            timer:sleep(100),
            io:format("ping at N=~w got N=~w~n",[N,D]),
            pong ! D-1,
            ping_loop(N+1)
    after
        20000 ->
            ok
    end.
pong_loop() ->
    receive
        stop ->
            ok;
        0 ->
            io:format("pong got N=0");
        D ->
            timer:sleep(100),
            io:format("pong got N=~w~n",[D]),
            ping ! D-1,
            pong_loop()
    after
        20000 ->
            ok
    end.

play(N) ->
    pong ! N.

stop() ->
    ping ! stop,
    pong ! stop.
