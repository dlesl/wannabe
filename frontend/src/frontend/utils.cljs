(ns frontend.utils)

(defn debounced [f interval]
  (let [last-fire (atom 0)]
    (fn [& args]
      (let [now (js/performance.now)]
        (when (> (- now @last-fire) interval)
          (reset! last-fire now)
          (apply f args))))))
