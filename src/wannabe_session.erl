-module(wannabe_session).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([ ensure_started/1
        , add_user/4
        , get_users/1
        , start_game/1
        , move/3
        , chat/3
        , drag/5
        , drag_end/2
        , again/1
        ]).

-record(state, { users = #{}
               , pids = #{}
               , game_state
               , idle_timer
               }).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(IDLE_TIMEOUT, timer:minutes(15)).

ensure_started(Name) ->
  case gen_server:start({global, Name}, ?MODULE, Name, []) of
    {ok, _} ->
      ?LOG_INFO([ {event, new_game}
                , {name, Name}
                ]),
      ok;
    {error, {already_started, _}} ->
      ok
  end.

add_user(Name, User, Token, Pid) ->
  gen_server:call({global, Name}, {add_user, User, Token, Pid}).

get_users(Name) ->
  gen_server:call({global, Name}, get_users).

start_game(Name) ->
  gen_server:call({global, Name}, start_game).

move(Name, User, Move) ->
  gen_server:call({global, Name}, {move, User, Move}).

chat(Name, User, Msg) ->
  gen_server:call({global, Name}, {chat, User, Msg}).

drag(Name, User, CardId, Dx, Dy) ->
  gen_server:call({global, Name}, {drag, User, CardId, Dx, Dy}).

drag_end(Name, User) ->
  gen_server:call({global, Name}, {drag_end, User}).

again(Name) ->
  gen_server:call({global, Name}, again).

%% gen_server implementation

init(Name) ->
  wannabe_matchmaker:register(self(), Name),
  {ok, #state{idle_timer=erlang:send_after(?IDLE_TIMEOUT, self(), idle_timeout)}}.

handle_call({add_user, User, Token, Pid}, _From, State=#state{users=Users0, pids=Pids0}) ->
  case Users0 of
    #{User := Token} ->
      monitor(process, Pid),
      Pids = Pids0#{Pid => User},
      {reply, ok, on_update_state(State#state{pids=Pids})};
    #{User := _} ->
      {reply, {error, incorrect_token}, State};
    #{} ->
      case State#state.game_state of
        undefined ->
          Users = Users0#{User => Token},
          monitor(process, Pid),
          Pids = Pids0#{Pid => User},
          {reply, ok, on_update_state(State#state{users=Users, pids=Pids})};
        _ ->
          {reply, {error, already_started}, State}
      end
  end;
handle_call(get_users, _From, State=#state{users=Users}) ->
  {reply, maps:keys(Users), State};
handle_call(start_game, _From, State=#state{users=Users}) ->
  wannabe_matchmaker:deregister(self()),
  {reply, ok, on_update_state(State#state{game_state=wannabe:new(maps:keys(Users))})};
handle_call({move, User, Move}, _From, State) ->
  case wannabe:move(State#state.game_state, User, Move) of
    {ok, NewGameState} ->
      {reply, ok, on_update_state(State#state{game_state=NewGameState})};
    Err ->
      {reply, Err, on_update_state(State)}
  end;
handle_call({chat, User, Msg}, _From, State) ->
  broadcast_chat(State, User, Msg),
  {reply, ok, State};
handle_call({drag, User, CardId, Dx, Dy}, _From, State) ->
  broadcast_drag(State, User, CardId, Dx, Dy),
  {reply, ok, State};
handle_call({drag_end, User}, _From, State) ->
  broadcast_drag_end(State, User),
  {reply, ok, State};
handle_call(again, _From, State=#state{users=Users}) ->
  {reply, ok, on_update_state(State#state{game_state=wannabe:new(maps:keys(Users))})}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(idle_timeout, State) ->
  {stop, normal, State};
handle_info({'DOWN', _, process, Pid, _}, State=#state{pids=Pids0, users=Users0})
  when is_map_key(map_get(Pid, Pids0), Users0) ->
  #{Pid := User} = Pids0,
  ?LOG_INFO([ {event, remove_listener}
            , {pid, Pid}
            , {user, User}
            ]),
  Pids = maps:remove(Pid, Pids0),
  Users = case State#state.game_state of
            undefined ->
              case lists:any(fun(V) -> V =:= User end, maps:values(Pids)) of
                true -> Users0;
                false -> maps:remove(User, Users0)
              end;
            _Started ->
              %% can't kick anyone out once the game has started
              Users0
          end,
  {noreply, on_update_state(State#state{pids=Pids, users=Users})};
handle_info(Info, State) ->
  ?LOG_WARNING([ {event, unexpected_info}
               , {info, Info}
               ]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

on_update_state(State) ->
  broadcast_state(State),
  new_timer(State).

broadcast_state(State=#state{pids=Pids}) ->
  [Pid ! {state, encode_state(State, User)} || {Pid, User} <- maps:to_list(Pids)].

broadcast_chat(#state{pids=Pids}, UserFrom, Msg) ->
  [Pid ! {chat, UserFrom, Msg} || {Pid, _User} <- maps:to_list(Pids)].

broadcast_drag(#state{pids=Pids, game_state=#{next_player := UserFrom}}, UserFrom, CardId, Dx, Dy) ->
  [Pid ! {drag, CardId, Dx, Dy} || {Pid, User} <- maps:to_list(Pids), User =/= UserFrom];
broadcast_drag(_, _, _, _, _) ->
  ignored.

broadcast_drag_end(#state{pids=Pids, game_state=#{next_player := UserFrom}}, UserFrom) ->
  [Pid ! drag_end || {Pid, User} <- maps:to_list(Pids), User =/= UserFrom];
broadcast_drag_end(_, _) ->
  ignored.

encode_state(#state{game_state=undefined, users=Users}, _User) ->
  {waiting, maps:keys(Users)};
encode_state(#state{game_state=GameState}, User) ->
  {playing, wannabe:view(GameState, User)}.

new_timer(State=#state{idle_timer=TimerRef}) ->
  erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
  State#state{idle_timer=erlang:send_after(timeout(State), self(), idle_timeout)}.

timeout(#state{game_state=undefined, users=Users}) when Users =:= #{} ->
  0; %% nothing to lose
timeout(#state{}) ->
  ?IDLE_TIMEOUT.
