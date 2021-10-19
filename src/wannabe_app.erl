%%%-------------------------------------------------------------------
%% @doc wannabe public API
%% @end
%%%-------------------------------------------------------------------

-module(wannabe_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  logger:set_primary_config(level, info),
  Dispatch = cowboy_router:compile([{'_', [ {"/", cowboy_static, {priv_file, wannabe, "index.html"}}
                                          , {"/games", wannabe_matchmaker_h, #{}}
                                          , {"/games/:game_id/:user_id/:token", wannabe_ws_h, #{}}
                                          , {"/js/[...]", cowboy_static, {priv_dir, wannabe, "js"}}
                                          , {"/css/[...]", cowboy_static, {priv_dir, wannabe, "css"}}
                                          , {"/svg/[...]", cowboy_static, {priv_dir, wannabe, "svg"}}
                                          ]}]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
  wannabe_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%% internal functions
