(ns frontend.socket
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [goog.string :as string]
   [cljs.core.async :refer [<! >! timeout chan close!]]
   [wscljs.client :as ws]
   [wscljs.format :as wsfmt]))

(defn ^:private ws-base []
  (let [host (-> js/window .-location .-host)
        pathname (-> js/window .-location .-pathname)]
    (case (-> js/window .-location .-protocol)
      "http:" (str "ws://" host pathname)
      "https:" (str "wss://" host pathname))))

(defn ^:private new-socket [game user token]
  (let [opened-chan (chan)
        closed-chan (chan)
        output-chan (chan)
        socket (ws/create
                (str (ws-base) "games/"
                     (string/urlEncode game) "/"
                     (string/urlEncode user) "/"
                     token)
                {:on-message #(let [data (.-data %)]
                                (go (>! output-chan data)))
                 :on-open #(close! opened-chan)
                 :on-close #(let [reason (-> % .-reason)]
                              (close! opened-chan)
                              (go (>! closed-chan reason)
                                  (close! closed-chan)))})]
    [socket opened-chan closed-chan output-chan]))

(defn make-socket [game user]
  (let [token (str (random-uuid))
        send-chan (chan)
        recv-chan (chan)
        connect #(new-socket game user token)]
    (go-loop [[socket opened-chan closed-chan output-chan :as args] (connect)]
      (<! opened-chan)

      (alt!
        output-chan ([data]
                     (let [parsed (-> data js/JSON.parse (js->clj :keywordize-keys true))]
                       (>! recv-chan [:message parsed])
                       (recur args)))
        closed-chan ([reason]
                       ;; reconnect unless it's fatal
                     (js/console.log "closed!" (str reason))
                     (when (try
                             (let [parsed (-> reason js/JSON.parse (js->clj :keywordize-keys true))]
                               (>! recv-chan [:closed parsed])
                               (not= (:command parsed) "fatal_error"))
                             (catch js/Object _
                               true))
                       (recur (connect))))
        send-chan ([msg] (when msg ;; stop if nil (closed)
                           (try (ws/send socket msg wsfmt/json) (catch js/Object _ nil))
                           (recur args))))
      (close! recv-chan))
    [send-chan recv-chan]))
