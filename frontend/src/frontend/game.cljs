(ns frontend.game
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [frontend.drag :as drag]))

(def all-colours [:red :yellow :green :blue :white])
(def all-values (range 1 6))
(def card-width 210)
(def card-height 297)
(def card-gap 20)
(def max-tokens {:storms 3
                 :notes 8})
(def token-radius 50)
(def token-gap 10)
(def player-height 400)
(def slide-in-time 1000)
(def game-width 1500)
(def game-height 2668)
(def green "#2d9c3a")

(defn ^:private translate [x y child]
  [:g {:transform (str "translate(" x " " y ")")}
   child])

(defn ^:private card [{:keys [colour value]}]
  (let [src (str "svg/" (or value "unknown") ".svg")
        bg (or colour "grey")]
    [:g
     [:rect
      {:width card-width
       :height card-height
       :rx 15
       :style {:fill bg
               :stroke-width 2
               :stroke "black"}}]
     [:image
      {:href src
       :width card-width
       :height card-height}]]))

(defn ^:private cards [drag-state player-cards their-turn? on-hint]
  (r/with-let [seen-card-ids-atom (r/atom #{})]
    (js/setTimeout #(swap! seen-card-ids-atom into (map :id player-cards)) slide-in-time)
    (let [seen-card-ids @seen-card-ids-atom]
      [:<>
       [:rect {:width (+ (* (count player-cards) (+ card-width card-gap)) (* 2 card-gap))
               :height (+ card-height (* card-gap 2))
               :style {:stroke-width (if their-turn? 8 2)
                       :stroke "black"
                       :fill "none"}}]
       (for [[idx {:keys [id on-action] :as card-info}] (map-indexed vector player-cards)
             :let [slide-in (when (not (seen-card-ids id)) "slide-in")]]
         ^{:key id}
         [translate (+ card-gap (* (+ card-width card-gap) idx)) card-gap
          (if on-action
            [drag/draggable drag-state
             {:child [card card-info]
              :class slide-in
              :id id
              :on-drop #(on-action %)}]
            [drag/dragged drag-state {:child [card card-info]
                                      :class slide-in
                                      :id id}])])
       (when on-hint
         [:text {:x (+ (* (count player-cards) (+ card-width card-gap)) (* 4 card-gap))
                 :y 200
                 :font-size 50
                 :on-click on-hint}
          "Hint"])])))

(defn ^:private tokens [pos type game-state]
  (let [used (type game-state)
        total (type max-tokens)]
    [:g
     (for [i (range total)]
       ^{:key i} [:circle {:cx (+ (:x pos) (* i (+ (* 2  token-radius) token-gap)))
                           :cy (+ (:y pos) (* 2 token-radius) token-gap)
                           :r token-radius
                           :style {:fill (if (>= i used) "black" "white")}}])]))

(defn ^:private discard [drag-state {:keys [notes]}]
  (r/with-let [active (r/atom false)]
    [drag/dropzone drag-state {:name :discard
                               :child [:g [:rect
                                           {:width 200
                                            :height 400
                                            :fill "none"
                                            :stroke-width 2
                                            :stroke (if @active "red" "black")}]
                                       [:text {:y 200 :style {:font-size 50}} "Discard"]]
                               :on-enter #(when (> 8 notes) (reset! active true))
                               :on-leave #(reset! active false)}]))

(defn ^:private fireworks-inner [fireworks highlight?]
  [:<>
   [:rect {:width (+ (* (count all-colours) (+ card-width card-gap)) (* 2 card-gap))
           :height (+ card-height (* card-gap (+ 2 (apply max all-values))))
           :style {:stroke-width 2
                   :stroke (if highlight? "red" "black")
                   :fill "none"}}]
   (for [[idx colour] (map-indexed vector all-colours)]
     (when-let [value (colour fireworks)]
       (for [value (range 1 (inc value))]
         ^{:key (str colour value)}
         [translate (+ card-gap (* (+ card-width card-gap) idx)) (* value card-gap)
          [card {:colour colour :value value}]])))])

(defn ^:private fireworks [drag-state {fireworks :fireworks}]
  (r/with-let [active (r/atom false)]
    [drag/dropzone drag-state {:name :play
                               :child [fireworks-inner fireworks @active]
                               :on-enter #(reset! active true)
                               :on-leave #(reset! active false)}]))

(defn ^:private player [drag-state {:keys [player-id player-cards their-turn? on-hint on-move]}]
  (let [player-cards
        (if on-move
          (map (fn [[n card-info]]
                 (assoc card-info :on-action #(on-move {:action % :card (inc n)})))
               (map-indexed vector player-cards))
          player-cards)]
    [:<>
     [:text player-id]
     [translate 0 20
      [cards drag-state player-cards their-turn? on-hint]]]))

(defn ^:private hint-dialog [player-id on-hint on-close]
  [:<>
   [:rect {:fill green :width game-width :height game-height :on-click on-close}]
   [translate 20 100
    [:<>
     [:text {:font-size 70} (str "Hint for player: " (name player-id))]
     (for [[idx [type value]] (map-indexed vector
                                           (concat (map (fn [c] [:colour c]) all-colours)
                                                   (map (fn [v] [:value v]) all-values)))]
       ^{:key value} [:text {:font-size 70
                             :x 20
                             :y (* (inc idx) 200)
                             :on-click #(on-hint type value)}
                      (str "These are your " (if (keyword? value) (name value) value) "s")])
     [:text {:x 400 :y 2250 :font-size 100 :on-click on-close} "Cancel"]]]])

(defn game [drag-state user-id game-state on-move on-done]
  (r/with-let [show-hint-dialog (r/atom nil)]
    (let [our-turn? (= (:next_player game-state) user-id)
          players-height (* player-height (count (:players game-state)))]
      [:div {:style {:background-color green}}
       [:svg {:width "100%"
              :viewBox (str/join " " [0 0 game-width game-height])
              :style {:max-width (str (Math/ceil (* 100 (/ game-width game-height))) "vh")}}
        [:rect {:width 1500 :height 2668 :fill green :on-click (when (:game_over game-state) on-done)}]
        (if (:game_over game-state)
          [:<> [:text {:x 20 :y 1000 :font-size 70}
                ;; not a bug -- you always lose when you play wannabe
                (str "You lost with " (:score game-state) " points.")]
           [:text {:x 20 :y 1200 :font-size 70 :on-click on-done} "Again?"]]
          [:<>
           [tokens {:x 80 :y 20} :notes game-state]
           [tokens {:x 1100 :y 20} :storms game-state]
           (for [[idx [player-id player-cards]] (map-indexed vector (:players game-state))
                 :let [player-id (name player-id)]]
             ^{:key idx}
             [translate 20 (+ 250 (* idx player-height))
              [player
               drag-state
               {:player-id player-id
                :player-cards player-cards
                :their-turn? (= (:next_player game-state) player-id)
                :on-move (when (and our-turn? (= player-id user-id))
                           on-move)
                :on-hint (when (and our-turn? (not= player-id user-id))
                           #(reset! show-hint-dialog player-id))}]])
           [translate 1250 (+ 300 players-height)
            [discard drag-state game-state]]
           [translate 20 (+ 300 players-height)
            [fireworks drag-state game-state]]
           [drag/last-item drag-state]
           (when-let [hint-for-player @show-hint-dialog]
             [hint-dialog
              hint-for-player
              (fn [type value] (on-move {:action :hint
                                         :type type
                                         :player_id (name hint-for-player)
                                         :value value})
                (reset! show-hint-dialog nil))
              #(reset! show-hint-dialog nil)])])]])))
