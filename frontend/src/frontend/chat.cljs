(ns frontend.chat
  (:require
   [reagent.core :as r]))

(defn chat [expanded? history on-send on-toggle]
  (r/with-let [text (r/atom "")]
    [:div.panel.chat
     [:p.panel-heading {:on-click on-toggle
                        :style {:cursor "pointer"}}
      "Chat"]
     (when expanded?
       [:<> (for [[i msg] (map-indexed vector history)]
              ^{:key i} [:p.panel-block msg])
        [:form {:on-submit (fn [e]
                             (.preventDefault e)
                             (-> e .-target .-message .-value on-send)
                             (reset! text ""))}
         [:input.input {:id "message"
                        :value @text
                        :on-change #(reset! text (-> % .-target .-value))}]]])]))
