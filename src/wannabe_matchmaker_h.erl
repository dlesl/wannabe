-module(wannabe_matchmaker_h).

-export([init/2, content_types_provided/2]).

-export([game_list/2]).

-define(MAX_GAMES, 10).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, game_list}], Req, State}.

game_list(Req, State) ->
  Body = jsx:encode(wannabe_matchmaker:list(?MAX_GAMES)),
  {Body, Req, State}.
