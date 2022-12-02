# rebar3_xref_check_app_calls

A rebar plugin description

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_xref_check_app_calls, "~> 0.0.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 rebar3_xref_check_app_calls
===> Fetching rebar3_xref_check_app_calls
===> Compiling rebar3_xref_check_app_calls
<Plugin Output>
```
