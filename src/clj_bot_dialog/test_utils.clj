(ns clj-bot-dialog.test-utils
  (:require [clj-bot-dialog.bot :as b]
            [clj-bot-dialog.lang :as l]
            [clojure.core.async :refer [<! <!! >! >!! chan close! go]]
            [clojure.test :refer [is]]
            [morse.api :as t]))

(defn simulate-dialog
  [dialog events]
  (let [chat      {:id :test-chat}
        user-chan (chan)
        bot-chan  (chan)
        results   (atom [])]
    (with-redefs [b/channels (atom {(:id chat) user-chan})
                  t/send-text (fn [_ chat-id text]
                                (swap! results conj {:from :bot
                                                     :user-id chat-id
                                                     :text text})
                                (>!! bot-chan {:user-id chat-id :text text}))]
      (binding [l/*dialog-channel* user-chan]
        (dialog chat))
      (doseq [e events]
        (case (:from e)
          :user (do
                  (swap! results conj e)
                  (>!! user-chan (:text e)))
          :bot  (let [{:keys [user-id text]} (<!! bot-chan)]
                  (swap! results conj {:from :bot
                                       :user-id user-id
                                       :text text})
                  (is (= (:text e) text)
                      (format "Expected bot to send '%s', but got '%s'"
                              (:text e) text)))))
      (close! user-chan)
      @results)))

(defn start-bot-mock
  [dialogs-map unknown-dialog not-in-dialog input-chan]
  (reset! b/channels {})
  (let [handler
        (fn [{:keys [chat text]}]
          (let [id (:id chat)]
            (cond
              (b/in-dialog? id)
              (go (>! (@b/channels id) text))

              (b/existing-command? text (keys dialogs-map))
              ((dialogs-map (b/command-name text)) chat)

              (b/command? text)
              (unknown-dialog chat)

              :else
              (not-in-dialog chat))))]

    (go
      (loop []
        (when-let [msg (<! input-chan)]
          (handler {:chat {:id   (:user-id msg)}
                    :text (:text msg)})
          (recur))))))

(defn simulate-scenario
  [dialogs-map unknown-dialog not-in-dialog events & {:keys [net-delay-fn]}]
  (let [input-chan  (chan)
        output-chan (chan 1024)
        user-events (->> events (filter #(= :user (:from %))) (sort-by :time))
        bot-events  (->> events (filter #(= :bot  (:from %))) (sort-by :time))
        expected-n  (count bot-events)
        origin      (System/currentTimeMillis)]

    (with-redefs [t/send-text
                  (fn [_ user-id text]
                    (when net-delay-fn
                      (Thread/sleep 100))
                    (let [t-ms (- (System/currentTimeMillis) origin)]
                      (>!! output-chan
                           {:user-id user-id
                            :text    text
                            :time-ms t-ms})))]
      (start-bot-mock
        dialogs-map unknown-dialog not-in-dialog input-chan)

      (future
        (doseq [{:keys [time user-id text]} user-events]
          (let [delay (- time
                         (- (System/currentTimeMillis) origin))]
            (when (pos? delay)
              (Thread/sleep delay)))
          (>!! input-chan {:user-id user-id :text text}))
        (close! input-chan))

      (let [actuals
            (loop [acc []]
              (if (< (count acc) expected-n)
                (let [m (<!! output-chan)]
                  (if m
                    (recur (conj acc m))
                    (do
                      (is m
                          (format "Expected %d bot messages, got only %d"
                                  expected-n (count acc)))
                      acc)))
                acc))

            actuals-by-user
            (atom
              (reduce
                (fn [m act]
                  (update m (:user-id act) (fnil conj []) act))
                {}
                actuals))]


        (doseq [[idx {:keys [user-id text time]}]
                (map-indexed vector bot-events)]
          (let [q (get @actuals-by-user user-id [])]
            (if (empty? q)
              (is false
                  (format "No bot message for expected #%d %s"
                          idx {:user-id user-id :text text}))
              (let [{:keys [time-ms] :as act} (first q)]

                (swap! actuals-by-user assoc user-id (subvec (vec q) 1))

                (is (= text (:text act))
                    (format "Text mismatch for user %d at event #%d: expected \"%s\", got \"%s\""
                            user-id idx text (:text act)))

                (is (<= time-ms time)
                    (format "Bot msg #%d for user %d too late: %dms > %dms"
                            idx user-id time-ms time))))))

        (doseq [[uid q] @actuals-by-user
                act    q]
          (is false
              (format "Unexpected extra bot message for user %d: %s"
                      uid act)))))))
