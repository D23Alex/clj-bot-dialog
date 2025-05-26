(ns clj-bot-dialog.bot
  (:require [clj-bot-dialog.send :refer [set-pool-size!]]
            [clojure.core.async :refer [>! go]]
            [clojure.string :as str]
            [morse.handlers :as h]
            [morse.polling :as p]))

(def channels (atom {}))

(defn in-dialog? [id]
  (contains? @channels id))

(defn command? [text]
  (clojure.string/starts-with? text "/"))

(defn command-name [command]
  (subs command 1))

(defn existing-command? [command command-names]
  (and (command? command)
       (contains? (set command-names) (command-name command))))

(defn start-bot
  ([token dialogs-map unknown-dialog not-in-dialog]
   (start-bot token dialogs-map unknown-dialog not-in-dialog {}))
  ([token dialogs-map unknown-dialog not-in-dialog {:keys [send-pool-size] :or {send-pool-size 4}}]
   ;; размер пула
   (set-pool-size! send-pool-size)
   ;; если был старый пул останавливаем его
   (when-let [pool @clj-bot-dialog.send/pool]
     (.shutdownNow pool)
     (reset! clj-bot-dialog.send/pool nil))

   (when (str/blank? token)
     (println "Please provide a valid token!")
     (System/exit 1))

   (h/defhandler handler
                 (h/message-fn
                   (fn [{{id :id} :chat text :text chat :chat :as message}]
                     (cond
                       (in-dialog? id)
                       (go (>! (@channels id) text))

                       (existing-command? text (keys dialogs-map))
                       ((dialogs-map (command-name text)) chat)

                       (command? text)
                       (unknown-dialog chat)

                       :else
                       (not-in-dialog chat)))))

   (println "Starting bot with" send-pool-size "send-threads")
   (loop [c (p/start token handler)]
     (Thread/sleep 500)
     (if (.closed? c)
       (recur (p/start token handler))
       (recur c)))))