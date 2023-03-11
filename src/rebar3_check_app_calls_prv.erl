%%% @doc Plugin provider for rebar3 check_app_calls.
-module(rebar3_check_app_calls_prv).

-export([init/1, do/1]).

-ignore_xref([do/1,
              {rebar_app_info, out_dir, 1},
              {rebar_app_info, applications, 1},
              {rebar_app_info, name, 1},
              {providers, create, 1},
              {rebar_state, add_provider, 2}]).

-define(PROVIDER, check_app_calls).
-define(DEPS, [compile]).
-define(OPTS, []).
-define(XREF_SERVER_NAME, ?MODULE).

-type application() :: atom().
-type xref_server() :: pid().
-type analysis_error() ::
    {call_non_dep | dep_not_called, {application(), application()}} |
    {undefined_call, mfa(), mfa()}.

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Desc = "Plugin that checks that you call all apps your app depends on, but no other.",
    Provider =
        providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, "rebar3 check_app_calls"},
                          {opts, ?OPTS},
                          {short_desc, "Checks calls to dependency applications"},
                          {desc, Desc}]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% TODO
%% Check args:
%% {Args, _} = rebar_state:command_parsed_args(State),
%% ?OPTS are documented here: http://rebar3.org/docs/tutorials/building_plugins/

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, Xref} = xref:start(?XREF_SERVER_NAME),

    ProjectApps = rebar_state:project_apps(State),
    AllRebarDeps = rebar_state:all_deps(State),
    AllDeps =
        ordsets:from_list(
            lists:flatmap(fun rebar_app_info:applications/1, ProjectApps)),
    NonRebarDeps = non_rebar_deps(AllRebarDeps, AllDeps),
    %% io:format("non rebar deps ~p", [NonRebarDeps]),
    xref:set_default(Xref,
                     [{warnings, rebar_state:get(State, xref_warnings, false)},
                      {verbose, rebar_log:is_verbose(State)}]),
    lists:foreach(fun(App) -> add_non_rebar_app(Xref, App) end, NonRebarDeps),
    lists:foreach(fun(App) -> add_app(Xref, App) end, ProjectApps ++ AllRebarDeps),
    case analysis(Xref, ProjectApps) of
        [] ->
            {ok, State};
        Errors ->
            {error,
             lists:flatten(
                 lists:map(fun format_error/1, Errors))}
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec add_non_rebar_app(xref_server(), application()) -> ok.
add_non_rebar_app(Xref, App) ->
    {ok, _Res} = xref:add_application(Xref, code:lib_dir(App)),
    ok.

-spec non_rebar_deps([rebar_app_info:t()], ordsets:ordset(application())) ->
                        [application()].
non_rebar_deps(RebarAppInfos, AllDeps) ->
    RebarApplications =
        ordsets:from_list(
            lists:map(fun(App) ->
                         Name = rebar_app_info:name(App),
                         binary_to_atom(Name)
                      end,
                      RebarAppInfos)),
    ordsets:subtract(AllDeps, RebarApplications).

-spec format_error(analysis_error()) -> io_lib:chars().
format_error({undefined_call, {From, To}}) ->
    io_lib:format("Call from ~p to non dependency ~p~n", [From, To]);
format_error({call_non_dep, {From, To}}) ->
    io_lib:format("Call from ~p to non dependency ~p~n", [From, To]);
format_error({dep_not_called, {From, To}}) ->
    io_lib:format("Dependency ~p not called fron ~p~n", [To, From]).

-spec add_app(xref_server(), rebar_app_info:t()) -> ok.
add_app(Xref, App) ->
    OutDir = rebar_app_info:out_dir(App),
    {ok, _Res} = xref:add_application(Xref, OutDir),
    % io:format("Add app Dir ~p~nRes ~p~n", [OutDir, _Res]),
    ok.

-spec analysis(xref_server(), [rebar_app_info:t()]) -> [analysis_error()].
analysis(Xref, Apps) ->
    lists:flatmap(fun(App) -> analyze_app(Xref, App) end, Apps).

-spec analyze_app(xref_server(), rebar_app_info:t()) -> [analysis_error()].
analyze_app(Xref, App) ->
    Name = rebar_app_info:name(App),
    NameAtom = binary_to_atom(Name),
    Deps =
        ordsets:from_list(
            rebar_app_info:applications(App)),
    {ok, UndefinedCalls} =
        xref:q(Xref, "(XC - UC) || (XU - X - B) | " ++ binary_to_list(Name) ++ ":App"),

    {ok, Calls} = xref:q(Xref, "AE | " ++ binary_to_list(Name)),
    {_, Called0} = lists:unzip(Calls),
    Called = ordsets:from_list(Called0),
    %% io:format("App ~p~nDeps ~p~nCalled ~p~n~n", [Name, Deps, Called]),
    OkNotCalled = ordsets:from_list([kernel, stdlib]),
    CallNonDep = ordsets:subtract(Called, ordsets:add_element(NameAtom, Deps)),
    DepNotCalled = ordsets:subtract(Deps, ordsets:union(OkNotCalled, Called)),
    [{undefined_call, Call} || Call <- UndefinedCalls]
    ++ [{call_non_dep, {NameAtom, Bad}} || Bad <- CallNonDep]
    ++ [{dep_not_called, {NameAtom, Bad}} || Bad <- DepNotCalled].
