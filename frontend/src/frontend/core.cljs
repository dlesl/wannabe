(ns frontend.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [frontend.join-game :refer [join-game]]
   [frontend.session :refer [session]]))

(defn ^:private root []
  (let [current-game (r/atom nil)]
    (fn []
      (if-let [[user-id game-id] @current-game]
        [session user-id game-id #(do (js/alert %) (reset! current-game nil))]
        [join-game #(do (reset! current-game [%1 %2]))]))))

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [root] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
