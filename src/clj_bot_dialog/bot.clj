(ns clj-bot-dialog.bot
  (:require [clojure.core.async :refer [<!! >! go]]
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

(defn start-bot [token dialogs unknown-command-dialog not-in-dialog-dialog]
  (when (str/blank? token)
    (println "Please provide a valid token!")
    (System/exit 1))

  (h/defhandler handler
                (h/message-fn
                  (fn [{{id :id} :chat text :text chat :chat :as message}]
                    (cond
                      (in-dialog? id) (go (>! (@channels id) text))
                      (existing-command? text (keys dialogs)) ((dialogs (command-name text)) chat)
                      (command? text) (unknown-command-dialog chat)
                      :else (not-in-dialog-dialog chat)))))
  (println "Starting the test-bot")
  (loop [c (p/start token handler)]
    (Thread/sleep 500)
    (if (.closed? c)
      (recur (p/start token handler))
      (recur c))))
