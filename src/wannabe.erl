-module(wannabe).

-include_lib("stdlib/include/assert.hrl").

-type state() :: #{ players := #{player_id() => hand()}
                  , order := [player_id()]
                  , next_player := player_id()
                  , pile := [card()]
                  , notes := integer()
                  , storms := integer()
                  , fireworks := #{colour() => value()}
                  , game_ends_after => player_id()
                  , game_over => true
                  }.

-type view() :: #{ players := #{player_id() => [card()]}
                 , order := [player_id()]
                 , next_player := player_id()
                 , pile := integer() %% size of the pile
                 , notes := integer()
                 , storms := integer()
                 , fireworks := #{colour() => value()}
                 , game_ends_after => player_id()
                 , game_over => true
                 }.

-type hand() :: #{pos_integer() => card() | empty}.

-type card_id() :: integer().

-type card() :: #{ colour := colour()
                 , value := value()
                 , known := ordsets:ordset(colour | value)
                 , id := card_id()
                 }.

-type colour() :: red | yellow | green | blue | white. %% | colourful.

-type value() :: 1..5.

-type player_id() :: term().

-type move() :: {hint, player_id(), hint()}
              | {discard, integer()}
              | {play, integer()}.

-type hint() :: #{colour := colour()} | #{value := value()}.

-define(COLOURS, [red, yellow, green, blue, white]). %% , colourful]).
-define(VALUES, [1, 1, 1, 2, 2, 3, 3, 4, 4, 5]).
-define(MAX_NOTES, 8).
-define(MAX_STORMS, 3).

-export([ new/1
        , view/2
        , score/1
        , move/3
        ]).

-spec new([player_id()]) -> state().
new(PlayerIds) when length(PlayerIds) >= 2, length(PlayerIds) =< 5 ->
  ?assertEqual(length(lists:usort(PlayerIds)), length(PlayerIds)),
  Cards = shuffled_deck(),
  CardsPerPlayer = case length(PlayerIds) < 4 of
                     true -> 5;
                     false -> 4
                   end,
  {Pile, Players} =
    lists:foldl(fun(Id, {Pile0, Players}) ->
                    {PlayerCards, Pile} = lists:split(CardsPerPlayer, Pile0),
                    {Pile, Players#{Id => maps:from_list(enumerate(PlayerCards))}}
                end,
                {Cards, #{}},
                PlayerIds),
  #{ players => Players
   , order => PlayerIds
   , next_player => hd(PlayerIds)
   , pile => Pile
   , notes => ?MAX_NOTES
   , storms => 0
   , fireworks => #{}
   }.

-spec view(state(), player_id()) -> view().
view(#{players := Players0, pile := Pile}=State, ViewPlayerId) ->
  Players =
    maps:map(fun(CardsPlayerId, Cards) ->
                 [card_view(ViewPlayerId, CardsPlayerId, Card)
                  || {_Pos, Card} <- lists:sort(maps:to_list(Cards))]
             end,
             Players0),
  State#{ players := Players
        , pile := length(Pile)
        , score => score(State)
        }.

card_view(ViewPlayerId, ViewPlayerId, #{known := Known}=Card) ->
  maps:with([id|Known], Card);
card_view(_ViewPlayerId, _CardsPlayerId, #{known := _Known}=Card) ->
  maps:with([id, colour, value], Card).

-spec score(state()) -> integer().
score(#{fireworks := Fireworks}) ->
  lists:sum(maps:values(Fireworks)).

-spec move(state(), player_id(), move()) ->
        {ok, state()}
          | {error, game_over}
          | {error, not_your_turn}
          | {error, invalid_move}.
move(#{game_over := true}, _PlayerId, _Move) ->
  {error, game_over};
move(#{order := Order}=PrevState, PlayerId, Move) ->
  case do_move(PrevState, PlayerId, Move) of
    {ok, NextState} when map_get(game_ends_after, PrevState) =:= PlayerId ->
      {ok, NextState#{game_over => true}};
    {ok, #{storms := ?MAX_STORMS}=NextState} ->
      {ok, NextState#{game_over => true}};
    {ok, #{fireworks := #{red := 5, yellow := 5, green := 5, blue := 5, white := 5}}=NextState} ->
      {ok, NextState#{game_over => true}};
    {ok, NextState} ->
      {ok, NextState#{next_player := next_player(PlayerId, Order)}};
    Err ->
      Err
  end.

do_move(#{next_player := CurrId}, PlayerId, _Move) when CurrId =/= PlayerId ->
  {error, not_your_turn};
do_move(#{notes := 0}, _PlayerId, {hint, _OtherId, _Hint}) ->
  {error, invalid_move};
do_move(#{notes := Notes, players := Players}=State, _PlayerId, {hint, OtherId, Hint}) ->
  #{OtherId := OtherCards0} = Players,
  OtherCards = case Hint of
                 #{colour := Colour} -> update_known(colour, Colour, OtherCards0);
                 #{value := Value} -> update_known(value, Value, OtherCards0)
               end,
  {ok, State#{notes := Notes - 1, players := Players#{OtherId := OtherCards}}};
do_move(#{notes := ?MAX_NOTES}, _PlayerId, {discard, _}) ->
  {error, invalid_move};
do_move(#{pile := []}, _PlayerId, {discard, _}) ->
  {error, invalid_move};
do_move(#{notes := Notes}=State0, PlayerId, {discard, Idx}) ->
  State = replace_card(State0, PlayerId, Idx),
  {ok, State#{notes := Notes + 1}};
do_move(#{players := Players, fireworks := Fireworks0}=State0, PlayerId, {play, Idx}) ->
  case maps:get(PlayerId, Players) of
    #{Idx := empty} ->
      {error, invalid_move};
    #{Idx := #{colour := Colour, value := Value}} ->
      case maps:get(Colour, Fireworks0, 0) + 1 of
        Value ->
          State = State0#{fireworks := Fireworks0#{Colour => Value}},
          {ok, replace_card(State, PlayerId, Idx)};
        _ ->
          State = State0#{storms := maps:get(storms, State0) + 1},
          {ok, replace_card(State, PlayerId, Idx)}
      end
  end.

update_known(Key, ValueToMatch, Cards) ->
  maps:map(fun(_, #{Key := V, known := Known}=Card) when V =:= ValueToMatch ->
               Card#{known := ordsets:add_element(Key, Known)};
              (_, #{Key := _, known := _}=Card) ->
               Card
           end,
           Cards).

replace_card(#{players := Players}=State0, PlayerId, Idx) ->
  #{PlayerId := Cards} = Players,
  {Card, State} = draw_card(State0, PlayerId),
  State#{players := Players#{PlayerId := Cards#{Idx := Card}}}.

draw_card(#{pile := []}=State, _) ->
  {empty, State};
draw_card(#{pile := [LastCard]}=State, PlayerId) ->
  {LastCard, State#{game_ends_after => PlayerId}};
draw_card(#{pile := [H|T]}=State, _) ->
  {H, State#{pile := T}}.

shuffled_deck() ->
  AllCards = enumerate([{Colour, Value} || Colour <- ?COLOURS, Value <- ?VALUES]),
  shuffle([new_card(Id, Colour, Value) || {Id, {Colour, Value}} <- AllCards]).

shuffle(List) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- List])].

new_card(Id, Colour, Value) ->
  #{ colour => Colour
   , value => Value
   , id => Id
   , known => []
   }.

next_player(PlayerId, Order) ->
  case lists:dropwhile(fun(P) -> P =/= PlayerId end, Order) of
    [PlayerId, Next|_] -> Next;
    [PlayerId] -> hd(Order)
  end.

enumerate(List) ->
  lists:zip(lists:seq(1, length(List)), List).
