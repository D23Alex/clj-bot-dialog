(ns clj-bot-dialog.core-test
  (:require
    [clj-bot-dialog.lang :as l]
    [clojure.core.async :refer [<!! >!! chan close!]]
    [clojure.test :refer :all]
    [clj-bot-dialog.examples :as ex]
    [clj-bot-dialog.test-utils :refer [simulate-dialog simulate-scenario]]))

(deftest always-true
  (testing "always-true")
  (is (= 1 1)))

(deftest name-dialog-asks-for-name
  (testing "name-dialog спрашивает имя и завершает"
    (let [events
          [{:from :bot  :text "What is your name?"}
           {:from :user :text "Alice"}]]
      (simulate-dialog ex/name-dialog events))))

(deftest greet-dialog-greets-user
  (testing "greet-dialog после имени приветствует по имени"
    (let [events
          [{:from :bot  :text "What is your name?"}
           {:from :user :text "Bob"}
           {:from :bot  :text "Hello, Bob!"}]]
      (simulate-dialog ex/greet-dialog events))))

(deftest user-description-dialog-collects-all-fields
  (testing "user-description-dialog собирает данные и отдаёт сводку"
    (let [events
          [;; спрашиваем имя
           {:from :bot  :text "What is your name?"}
           {:from :user :text "Jane"}
           ;; спрашиваем возраст
           {:from :bot  :text "How old are you?"}
           {:from :user :text "30"}
           ;; спрашиваем откуда
           {:from :bot  :text "Where are you from?"}
           {:from :user :text "Paris"}
           ;; спрашиваем универ
           {:from :bot  :text "What university did you go to?"}
           {:from :user :text "Sorbonne"}
           ;; спрашиваем спорт
           {:from :bot  :text "What's your favorite sport?"}
           {:from :user :text "Tennis"}
           ;; спрашиваем музыку
           {:from :bot  :text "What's your favorite music genre?"}
           {:from :user :text "Jazz"}
           ;; спрашиваем про Telegram
           {:from :bot  :text "Do you like telegram?"}
           {:from :user :text "yes"}
           ;; итоговый вывод
           {:from :bot  :text "Thanks for sharing, here is what I know about you:"}
           {:from :bot
            :text  "Jane, you are 30 years old from Paris, you went to Sorbonne, you like Tennis and Jazz, and telegram bots as well"}]]
      (simulate-dialog ex/user-description-dialog events))))


(deftest parallel-name-and-greet
  (simulate-scenario
    {"name"  ex/name-dialog
     "greet" ex/greet-dialog}
    ex/unknown-command-dialog
    ex/not-in-dialog-dialog
    [{:from    :user  :time    0  :user-id 1 :text "/greet"}
     {:from    :bot   :time   100  :user-id 1 :text "What is your name?"}
     {:from    :user  :time   500  :user-id 1 :text "Alice"}
     {:from    :bot   :time  1000  :user-id 1 :text "Hello, Alice!"}

     {:from    :user  :time   200  :user-id 2 :text "/greet"}
     {:from    :bot   :time   300  :user-id 2 :text "What is your name?"}
     {:from    :user  :time   600  :user-id 2 :text "Bob"}
     {:from    :bot   :time  1100  :user-id 2 :text "Hello, Bob!"}]))

(deftest throughput-via-subdialog-timing-old
  (let [durations (atom [])
        timed-speed-dialog
        (l/dialog
          (let [t0  (System/currentTimeMillis)
                res (l/subdialog ex/speed-dialog)
                dt  (- (System/currentTimeMillis) t0)]
            (swap! durations conj dt)
            res))

        N              7
        process-delay  1000
        user-latency   900
        events
        (apply concat
               (for [uid (range 1 (inc N))]
                 (let [send-time (* uid 10)
                       [pairs _]
                       (reduce
                         (fn [[acc last-time] _]
                           (let [ping-time (+ last-time process-delay)
                                 user-time (+ ping-time  user-latency)]
                             [(into acc
                                    [{:from    :bot  :time ping-time :user-id uid :text "ping"}
                                     {:from    :user :time user-time :user-id uid :text "pong"}])
                              user-time]))
                         [[] send-time]
                         (range 10))]
                   ;; первая — это user-команда
                   (into [{:from    :user :time send-time :user-id uid :text "/speed"}]
                         pairs))))]

    (simulate-scenario
      {"speed" timed-speed-dialog}
      (fn [_] nil)
      (fn [_] nil)
      events
      :net-delay-fn #(rand-int 30))   ;; RTT 0–29 ms

    (let [ds    (sort @durations)
          cnt   (count ds)
          pct   (fn [p] (nth ds (dec (long (Math/ceil (* p cnt))))))
          p50   (pct 0.50)
          p95   (pct 0.95)
          p99   (pct 0.99)]
      (is (< p50 500) (format "P50=%dms >200ms" p50))
      (is (< p95 500) (format "P95=%dms >400ms" p95))
      (is (< p99 500) (format "P99=%dms >600ms" p99)))))

(defn run-scenario-with-timing
  [dialogs-map unknown-dialog not-in-dialog
   events percent-thresholds
   & {:keys [net-delay-fn]
      :or   {net-delay-fn (constantly 0)}}]
  (let [durations
        (atom [])
        wrapped-dialogs
        (into {}
              (for [[k dlg] dialogs-map]
                [k
                 (l/dialog
                   (let [t0  (System/currentTimeMillis)
                         res (l/subdialog dlg)
                         dt  (- (System/currentTimeMillis) t0)]
                     (swap! durations conj dt)
                     res))]))]
    (simulate-scenario
      wrapped-dialogs unknown-dialog not-in-dialog events
      :net-delay-fn net-delay-fn)
    (when (seq percent-thresholds)
      (let [ds (sort @ex/response-durations)
            n  (count ds)]
        (doseq [[p thresh] percent-thresholds]
          (let [idx  (long (Math/ceil (* p n)))
                val  (nth ds (dec idx))
                _ (println idx)
                _ (println val)
                _ (println thresh)]
            (is (< val thresh)
                (format "P%.0f=%dms >%dms" (* 100 p) val thresh))))))))


(defn ping-pong-events
  [user-ids bot-latency user-latency ping-count]
  (println (/ (+ bot-latency user-latency) (count user-ids)))
  (apply concat
         (for [uid user-ids]
           (let [start-time (* uid (int (/ (+ bot-latency user-latency) (count user-ids))))
                 [pairs _]
                 (reduce
                   (fn [[acc last-time] _]
                     (let [ping-time (+ last-time user-latency)
                           pong-time (+ ping-time bot-latency)]
                       [(into acc
                              [{:from :user :time ping-time :user-id uid :text "ping"}
                               {:from :bot :time pong-time :user-id uid :text "pong"}])
                        pong-time]))
                   [[] start-time]
                   (range ping-count))]
             (into
               [{:from    :user :time start-time :user-id uid :text "/speed"}]
               pairs)))))


(deftest throughput-via-subdialog-timing
  (println (System/getProperty "clojure.core.async.pool-size"))
  (let [N 5
        bot-latency 106
        user-latency (* bot-latency 5)
        _ (println bot-latency user-latency (int (/ (+ bot-latency user-latency) N)))
        ping-count 10
        events
        (ping-pong-events
          (range 1 (inc N))
          bot-latency
          user-latency
          ping-count)]
    (run-scenario-with-timing
      {"speed" ex/speed-dialog}
      (fn [_] nil)
      (fn [_] nil)
      events
      {0.5 1
       0.95 1
       0.99 1}
      :net-delay-fn #(rand-int 30))))


(deftest receive-timeout-default-path
  (let [dialog
        (l/dialog
          (l/with-timeout 500
                          (l/transmit "Ask?")
                          (l/receive)
                          (l/transmit "Timed-out!")))
        events
        [{:from    :user :time 0  :user-id 1 :text "/t0"}
         {:from :bot  :time 200  :user-id 1 :text "Ask?"}]]
    (run-scenario-with-timing
      {"t0" dialog}
      (fn [_] nil)
      (fn [_] nil)
      events
      {})))

(deftest nested-timeout-behavior
  (let [dialog
        (l/dialog
          (l/with-timeout 1000
                          (l/transmit "A")
                          (l/with-timeout 500
                                          ;; этот receive отработает default-path и
                                          ;; весь остальной код во вложенном блоке скипнётся
                                          (l/receive)
                                          (l/transmit "Inner OK"))
                          ;; вот этот transmit B выполнится всегда
                          (l/transmit "B")))

        events
        [{:from    :user :time  0 :user-id 1 :text "/t1"}
         ;; ответ бота на первое transmit
         {:from    :bot  :time  500 :user-id 1 :text "A"}
         ;; ответ бота на внешний after-timeout
         {:from    :bot  :time 2000 :user-id 1 :text "B"}]]

    (run-scenario-with-timing
      {"t1" dialog}
      (fn [_] nil)
      (fn [_] nil)
      events
      {})))