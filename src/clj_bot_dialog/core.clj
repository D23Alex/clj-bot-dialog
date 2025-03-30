(ns clj-bot-dialog.core
  (:require [clj-bot-dialog.bot :as b]
            [clj-bot-dialog.examples :as e]
            [clj-bot-dialog.lang :as l])
  (:gen-class))

(defn -main
  [& args]
  (b/start-bot l/token
               {"name"        e/name-dialog
                "help"        e/help-dialog
                "greet"       e/greet-dialog
                "description" e/user-description-dialog}
               e/unknown-command-dialog
               e/not-in-dialog-dialog))
