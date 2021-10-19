(ns frontend.join-game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(defn join-game [on-join]
  (r/with-let [game-name (r/atom (str "game-" (rand-int 1000)))
               user-name (r/atom (str "player-" (rand-int 1000)))
               games (r/atom [])
               refresh-games #(go (reset! games (:body (<! (http/get "games" {:with-credentials? false})))))
               interval (do (refresh-games) (js/setInterval refresh-games 1000))]
    [:div.section
     [:div.container.is-max-desktop
      [:div.columns
       [:div.column
        [:div.panel
         [:p.panel-heading "Games"]
         (for [game @games]
           ^{:key game} [:a.panel-block {:on-click #(on-join game @user-name)} game])]]
       [:div.column
        [:div.content.box
         [:h1.title "Join a game!"]
         [:div.field
          [:label.label "Game name (existing or new)"]
          [:div.control
           [:input.input {:type "text"
                          :placeholder "My fun game"
                          :value @game-name
                          :on-change #(reset! game-name (-> % .-target .-value))}]]]
         [:div.field
          [:label.label "User name"]
          [:div.control
           [:input.input {:type "text"
                          :placeholder "Wannabe"
                          :value @user-name
                          :on-change #(reset! user-name (-> % .-target .-value))}]]]
         [:button.button.is-primary {:on-click #(on-join @game-name @user-name)} "Join"]]]]]]
    (finally (js/clearInterval interval))))
