(ns clj-bot-dialog.lang
  (:require [clojure.core.async :refer [<! chan close! go]]
            [environ.core :refer [env]]
            [morse.api :as t]
            [clj-bot-dialog.bot :as b]))

(def token (env :telegram-token))

(def ^:dynamic *chat* nil)
(def ^:dynamic *dialog-channel* nil)

(defmacro user-id []
  `(:id *chat*))

(defmacro transmit [text]
  `(t/send-text token (user-id) ~text))

(defmacro receive
  ([]
   `(<! (get @b/channels (user-id))))
  ([prompt]
   `(do (transmit ~prompt)
        (receive))))

(defmacro defdialog [& body]
  (let [chat-sym (gensym "chat")]
    `(fn [~chat-sym]
       (binding [*chat* ~chat-sym]
         (if *dialog-channel*
           (go
             ~@body)
           (let [new-dialog-chan# (chan)]
             (binding [*dialog-channel* new-dialog-chan#]
               (swap! b/channels assoc (user-id) new-dialog-chan#)
               (go
                 (try
                   ~@body
                   (finally
                     (close! new-dialog-chan#)
                     (swap! b/channels dissoc (user-id))))))))))))

(defmacro subdialog [dialog]
  `(<! (~dialog *chat*)))
