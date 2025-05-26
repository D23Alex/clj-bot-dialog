(ns clj-bot-dialog.examples
  (:require [clj-bot-dialog.lang :as l]
            [clj-bot-dialog.helpers :refer [plet]]
            [clojure.java.io :as io])
  (:import (java.util UUID)))

(def name-dialog
  (l/dialog
    (l/receive :prompt "What is your name?")))

(def greet-dialog
  (l/dialog
    (l/transmit (str "Hello, " (l/subdialog name-dialog) "!"))))

(def help-dialog
  (l/dialog (l/transmit "Help is on the way")))

(def unknown-command-dialog
  (l/dialog (l/transmit "Unknown command. Use /help for help.")))

(def not-in-dialog-dialog
  (l/dialog (l/transmit "You are not in a dialog right now.")))

(def user-description-dialog
  (l/dialog (plet [name (l/subdialog name-dialog)
                   age (l/receive :prompt "How old are you?")
                   from (l/subdialog (l/dialog (l/receive :prompt "Where are you from?")))
                   uni (l/receive :prompt "What university did you go to?")
                   sport (l/subdialog (l/dialog (l/receive :prompt "What's your favorite sport?")))
                   music (l/receive :prompt "What's your favorite music genre?")
                   likes-tg? (l/receive :prompt "Do you like telegram?")
                   user-description (str name
                                         ", you are " age
                                         " years old from " from
                                         ", you went to " uni
                                         ", you like " sport
                                         " and " music
                                         (if (= "yes" likes-tg?) ", and telegram bots as well"))]
                  (l/transmit "Thanks for sharing, here is what I know about you:")
                  (l/transmit user-description))))

(def response-durations (atom []))

(def speed-dialog
  (l/dialog
    (dotimes [_ 10]                           ;; 10 итераций
      (let [buf# (byte-array (* 1024 1000))]
        (count buf#))
      (let [f# (io/file (str (UUID/randomUUID) ".tmp"))
            data# (apply str (repeat (* 1024 1000) \0))]
        (spit f# data#)
        (.delete f#))
      (l/receive)
      (let [t0 (System/currentTimeMillis)]
        (l/transmit "pong")
        (swap! response-durations conj (- (System/currentTimeMillis) t0))))))


(l/dialog
  (plet [_ (l/transmit "What is your name?")
         name (l/receive :prompt "Enter name:")
         _ (l/transmit "where are you from?")
         from (l/receive :prompt "Enter place:")]
    (l/transmit (str "Hi " name " from " from  "!"))))

(l/dialog
  (l/transmit "Your username:"
              (l/user-id)
              "Your telegram id:"
              (l/user-id)))
