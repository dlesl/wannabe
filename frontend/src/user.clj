(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]))

(defn cljs []
  (shadow/repl :app))
