%%%-------------------------------------------------------------------
%% @doc server_active_mode public API
%% @end
%%%-------------------------------------------------------------------

-module(server_active_mode_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    server_active_mode_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
