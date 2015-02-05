-module(rester_app).

-behaviour(application).

-export([start/2, stop/1]).

%% @doc Starts rester application.
start(_Type, _Args) ->
  rester_sup:start_link().

%% @doc Stops rester application.
stop(_State) ->
  ok.