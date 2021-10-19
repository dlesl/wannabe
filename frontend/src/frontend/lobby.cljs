(ns frontend.lobby)

(defn lobby [users on-start]
  [:div "waiting"
   [:div (for [user users]
           ^{:key user} [:p user])]
   [:button.button
    {:on-click on-start
     :disabled (not (and (>= (count users) 2) (<= (count users) 5)))}
    "start game"]])
