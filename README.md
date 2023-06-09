# rebar3_check_app_calls

A rebar plugin that checks that you call all apps your app depends on, but no other.

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [rebar3_check_app_calls]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 check_app_calls
```
