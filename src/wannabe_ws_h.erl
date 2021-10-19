-module(wannabe_ws_h).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").

%% Standard callbacks.
-export([ init/2
        , websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).

-define(MAX_NAME_LEN, 100).
-define(WS_OPTS, #{idle_timeout => infinity, max_frame_size => 2048}).

init(#{bindings := Bindings}=Req, _State) ->
  {cowboy_websocket, Req, Bindings, ?WS_OPTS}.

websocket_init(#{game_id := Game, user_id := User, token := Token}=State)
  when byte_size(Game) =< ?MAX_NAME_LEN,
       byte_size(User) =< ?MAX_NAME_LEN ->
  wannabe_session:ensure_started(Game),
  case wannabe_session:add_user(Game, User, Token, self()) of
    ok ->
      {[], State};
    {error, Reason} ->
      {[{close, jsx:encode(#{command => fatal_error, reason => Reason})}], State}
  end.

websocket_handle({text, Msg}, State) ->
  handle(jsx:decode(Msg), State);
websocket_handle(Data, State) ->
  ?LOG_ERROR([ {event, unrecognised_data}
             , {data, Data}
             , {state, State}
             ]),
  {[], State}.

websocket_info({state, {waiting, Users}}, State) ->
  {[{text, jsx:encode(#{ command => state
                       , state => waiting
                       , users => Users
                       })}], State};
websocket_info({state, {playing, View}}, State) ->
  {[{text, jsx:encode(#{ command => state
                       , state => playing
                       , view => View
                       })}], State};
websocket_info({chat, User, Msg}, State) ->
  {[{text, jsx:encode(#{ command => chat
                       , from => User
                       , message => Msg
                       })}], State};
websocket_info({drag, CardId, Dx, Dy}, State) ->
  {[{text, jsx:encode(#{ command => drag
                       , card => CardId
                       , dx => Dx
                       , dy => Dy
                       })}], State};
websocket_info(drag_end, State) ->
  {[{text, jsx:encode(#{command => drag_end})}], State}.

handle(#{<<"command">> := <<"start">>}, #{game_id := Game}=State) ->
  wannabe_session:start_game(Game),
  {[], State};
handle(#{ <<"command">> := <<"chat">>
        , <<"message">> := Msg},
       #{ game_id := Game
        , user_id := User
        }=State) ->
  wannabe_session:chat(Game, User, Msg),
  {[], State};
handle(#{ <<"command">> := <<"move">>
        , <<"action">> := Action
        , <<"card">> := CardIdx
        },
       #{ game_id := Game
        , user_id := User
        }=State) when is_integer(CardIdx) andalso
                      (Action =:= <<"discard">> orelse Action =:= <<"play">>) ->
  case wannabe_session:move(Game, User, {binary_to_atom(Action), CardIdx}) of
    ok -> {[], State};
    {error, Reason} -> {[{text, jsx:encode(#{command => error, reason => Reason})}], State}
  end;
handle(#{ <<"command">> := <<"move">>
        , <<"action">> := <<"hint">>
        , <<"type">> := Type
        , <<"value">> := Value
        , <<"player_id">> := OtherPlayerId
        },
       #{ game_id := Game
        , user_id := User
        }=State) ->
  Hint = case Type of
           <<"colour">> ->
             Colour = case Value of
                        <<"red">> -> red;
                        <<"yellow">> -> yellow;
                        <<"green">> -> green;
                        <<"blue">> -> blue;
                        <<"white">> -> white
                                       %% <<"colourful">> -> colourful
                      end,
             #{colour => Colour};
           <<"value">> when is_integer(Value) -> #{value => Value}
         end,
  case wannabe_session:move(Game, User, {hint, OtherPlayerId, Hint}) of
    ok -> {[], State};
    {error, Reason} -> {[{text, jsx:encode(#{command => error, reason => Reason})}], State}
  end;
handle(#{ <<"command">> := <<"drag">>
        , <<"card">> := CardId
        , <<"dx">> := Dx
        , <<"dy">> := Dy
        },
       #{ game_id := Game
        , user_id := User
        }=State) when is_integer(CardId), is_number(Dx), is_number(Dy) ->
  wannabe_session:drag(Game, User, CardId, Dx, Dy),
  {[], State};
handle(#{<<"command">> := <<"drag_end">>},
       #{ game_id := Game
        , user_id := User
        }=State) ->
  wannabe_session:drag_end(Game, User),
  {[], State};
handle(#{<<"command">> := <<"again">>}, #{game_id := Game}=State) ->
  wannabe_session:again(Game),
  {[], State};
handle(#{<<"command">> := <<"keepalive">>}, State) ->
  {[], State}.
