(ns clj-bot-dialog.examples
  (:require [clj-bot-dialog.lang :as l]
            [clj-bot-dialog.helpers :refer [plet]]))

(def name-dialog
  (l/defdialog
    (l/receive "What is your name?")))

(def greet-dialog
  (l/defdialog
    (l/transmit (str "Hello, " (l/subdialog name-dialog) "!"))))

(def help-dialog
  (l/defdialog (l/transmit "Help is on the way")))

(def unknown-command-dialog
  (l/defdialog (l/transmit "Unknown command. Use /help for help.")))

(def not-in-dialog-dialog
  (l/defdialog (l/transmit "You are not in a dialog right now.")))

(def user-description-dialog
  (l/defdialog (plet [name (l/subdialog name-dialog)
                      age (l/receive "How old are you?")
                      from (l/receive "Where are you from?")
                      uni (l/receive "What university did you go to?")
                      sport (l/receive "What's your favorite sport?")
                      music (l/receive "What's your favorite music genre?")
                      likes-tg? (l/receive "Do you like telegram?")
                      user-description (str name
                                            ", you are " age
                                            " years old from " from
                                            ", you went to " uni
                                            ", you like " sport
                                            " and " music
                                            (if (= "yes" likes-tg?) ", and telegram bots as well"))]
                     (l/transmit "Thanks for sharing, here is what I know about you:")
                     (l/transmit user-description))))
