(ns frontend.session
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :refer [>! <! close!]]
   [reagent.core :as r]
   [frontend.socket :refer [make-socket]]
   [frontend.chat :refer [chat]]
   [frontend.lobby :refer [lobby]]
   [frontend.game :refer [game]]
   [frontend.drag :as drag]
   [frontend.utils :refer [debounced]]))

(def chat-history-size 5)

(defn session [game-id user-id on-leave]
  (r/with-let [state (r/atom nil)
               chat-expanded? (r/atom true)
               chats (r/atom (into [] (repeat chat-history-size \u00A0)))
               [send-chan recv-chan] (make-socket game-id user-id)
               drag-state (drag/init (debounced #(go (>! send-chan {:command :drag :card % :dx %2 :dy %3})) 20)
                                     #(go (>! send-chan {:command :drag_end})))
               _ (go-loop []
                   (when-let [[type payload] (<! recv-chan)]
                     (condp = type
                       :message
                       (let [{cmd :command} payload]
                         (case cmd
                           "state"
                           (do (when (and (= (:state @state) "waiting") (:state payload "playing"))
                                 (reset! chat-expanded? false))
                               (when (not= @state payload)
                                 (drag/end-remote-drag drag-state))
                               (reset! state payload))

                           "chat"
                           (do
                             (reset! chat-expanded? true)
                             (swap! chats
                                    #(into []
                                           (take-last chat-history-size
                                                      (conj % (str (:from payload) ": " (:message payload)))))))

                           "error"
                           (js/alert (:reason payload))

                           "drag"
                           (let [{:keys [card dx dy]} payload]
                             (drag/set-remote-drag drag-state {:id card :dx dx :dy dy}))

                           "drag_end"
                           (drag/end-remote-drag drag-state)

                           (js/console.log "Unknown command" (str payload)))
                         (recur))

                       :closed
                       (on-leave payload))))]
    [:div (case (:state @state)
            "waiting"
            [lobby (:users @state) #(go (>! send-chan {:command :start}))]

            "playing"
            [game
             drag-state
             user-id
             (:view @state)
             #(go (>! send-chan (merge % {:command :move})))
             #(go (>! send-chan {:command :again}))]

            [:div "loading or broken..."])

     [chat
      @chat-expanded?
      @chats
      #(go (>! send-chan {:command :chat :message %}))
      #(swap! chat-expanded? not)]]
    (finally (close! send-chan))))
