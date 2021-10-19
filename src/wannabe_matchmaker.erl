-module(wannabe_matchmaker).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([ start_link/0
        , register/2
        , deregister/1
        , list/1
        ]).

-record(state, {}).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Pid, Name) ->
  gen_server:cast(?SERVER, {register, Pid, Name}).

deregister(Pid) ->
  gen_server:cast(?SERVER, {deregister, Pid}).

list(Length) ->
  case ets:match(?MODULE, {'_', '$1'}, Length) of
    '$end_of_table' -> [];
    {List, _} -> [Name || [Name] <- List]
  end.

%% gen_server implementation

init([]) ->
  ets:new(?MODULE, [named_table]),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({register, Pid, Name}, State) ->
  monitor(process, Pid),
  ets:insert(?MODULE, {Pid, Name}),
  {noreply, State};
handle_cast({deregister, Pid}, State) ->
  ets:delete(?MODULE, Pid),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
  ?LOG_INFO([ {event, remove_game}
            , {pid, Pid}
            , {game, ets:lookup(?MODULE, Pid)}
            ]),
  ets:delete(?MODULE, Pid),
  {noreply, State};
handle_info(Info, State) ->
  ?LOG_WARNING([ {event, unexpected_info}
               , {info, Info}
               ]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
