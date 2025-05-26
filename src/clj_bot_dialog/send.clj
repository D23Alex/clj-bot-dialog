(ns clj-bot-dialog.send
  (:require [clojure.core.async :refer [<!! >!! chan]]
            [morse.api :as t])
  (:import (java.util.concurrent Executors)))

(def send-chan      (chan 1024))
(def ^:private pool-size (atom 1024))
(def pool      (atom nil))

(defn set-pool-size!
  [n]
  (reset! pool-size n))

(defn ensure-send-pool!
  "Ленивая инициализация"
  [token]
  (when-not @pool
    (let [n    @pool-size
          exec (Executors/newFixedThreadPool n)]
      (reset! pool exec)
      (dotimes [_ n]
        (.submit exec
                 ^Runnable
                 (fn []
                   (loop []
                     (when-let [{:keys [chat-id text ack]} (<!! send-chan)]
                       (try
                         (t/send-text token chat-id text)
                         (finally
                           (>!! ack true)))
                       (recur)))))))))