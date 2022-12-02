%%% @doc Main entry point for the rebar3 rebar3_xref_check_app_calls plugin.
-module(rebar3_xref_check_app_calls).

-export([init/1]).

-ignore_xref([init/1]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_xref_check_app_calls_prv:init(State),
    {ok, State1}.
