(ns clj-bot-dialog.lang
  (:require [clj-bot-dialog.bot :as b]
            [clj-bot-dialog.send :refer [ensure-send-pool! send-chan]]
            [clojure.core.async :refer [<! >! alts! chan close! go timeout]]
            [clojure.string :as str]
            [environ.core :refer [env]]
            [morse.api :as t])
  (:import (clojure.lang ExceptionInfo)
           (java.util.regex Pattern)))

(def token (env :telegram-token))

(def ^:dynamic *chat* nil)
(def ^:dynamic *dialog-channel* nil)

(defmacro user-id []
  `(:id *chat*))

(defmacro username []
  `(:username *chat*))

(defmacro transmit
  [& texts]
  `(do
     ;; лениво инициализируем пул при первой отправке
     (ensure-send-pool! token)
     (doseq [msg# [~@texts]]
       (let [ack# (chan)]
         (>! send-chan
             {:chat-id (user-id)
              :text    msg#
              :ack     ack#})
         (<! ack#)))))

(def ^:dynamic *timeout-chans* [])

(defn safe-split
  [s]
  (mapv str/trim (str/split (str s) #",")))

(defmacro with-timeout
  [duration & body]
  (let [ms-expr
        (if (and (vector? duration)
                 (= 2 (count duration)))
          (let [[n unit] duration
                mult (case unit
                       :ms             1
                       :millisecond    1
                       :milliseconds   1
                       :s              1000
                       :sec            1000
                       :second         1000
                       :seconds        1000
                       :m              (* 60 1000)
                       :min            (* 60 1000)
                       :minute         (* 60 1000)
                       :minutes        (* 60 1000)
                       :h              (* 60 60 1000)
                       :hour           (* 60 60 1000)
                       :hours          (* 60 60 1000)
                       (throw (ex-info (str "Unsupported time unit: " unit) {})))]
            `(* ~n ~mult))
          ;; иначе duration это число миллисекунд
          duration)]
    `(let [tch# (timeout ~ms-expr)]
       (binding [*timeout-chans* (conj *timeout-chans* tch#)]
         (try
           ~@body
           (catch ExceptionInfo e#
             (if (= :receive-timeout (:type (ex-data e#)))
               nil
               (throw e#))))))))

(defn run-receive*
  [user-ch token timeout-chans opts-map]
  (let [{:keys [prompt pattern transform
                choices choice-fn show-choices?
                min max validate
                error-msg retry default

                on-pattern    pattern-retry-msg   pattern-default   pattern-default-msg
                on-transform  transform-retry-msg transform-default transform-default-msg
                on-validate   validate-retry-msg  validate-default  validate-default-msg
                on-bounds     bounds-retry-msg    bounds-default    bounds-default-msg]}
        (merge
          {:transform     identity
           :choice-fn     str
           :show-choices? true
           :validate      (fn [_] true)
           :error-msg     "Неверный ввод, попробуйте снова."
           :retry         3
           :default       nil
           :min           1
           :max           1
           :on-pattern    :retry
           :on-transform  :retry
           :on-validate   :retry
           :on-bounds     :retry}
          opts-map)

        res-ch (chan)]
    (go
      (try
        (let [prm         (or pattern-retry-msg error-msg)
              pdef        (if (nil? pattern-default) default pattern-default)
              pdm         pattern-default-msg

              trm         (or transform-retry-msg error-msg)
              tdef        (if (nil? transform-default) default transform-default)
              tdm         transform-default-msg

              vrm         (or validate-retry-msg error-msg)
              vdef        (if (nil? validate-default) default validate-default)
              vdm         validate-default-msg

              bmsg        (or bounds-retry-msg error-msg)
              bdef        (if (nil? bounds-default) default bounds-default)
              bdm         bounds-default-msg

              choice-str  (when (and choices show-choices?)
                            (let [ins (if (map? choices)
                                        (keys choices)
                                        choices)]
                              (str "(" (str/join "/" ins) ")")))
              full-prompt (if choice-str (str prompt " " choice-str) prompt)

              result
              (loop [cnt retry]
                (when (and full-prompt (not (str/blank? full-prompt)))
                  (t/send-text token (user-id) full-prompt))

                (let [raw
                      (loop []
                        (let [[v ch] (alts! (conj timeout-chans user-ch))]
                          (cond
                            (some #{ch} timeout-chans)
                            (throw (ex-info "receive timed out"
                                            {:type :receive-timeout}))

                            (and (= ch user-ch) (nil? v))
                            (recur)

                            :else
                            v)))]

                  (if (and (instance? Pattern pattern)
                           (not (re-matches pattern raw)))
                    (case on-pattern
                      :retry   (do
                                 (t/send-text token (user-id) prm)
                                 (if (pos? (dec cnt))
                                   (recur (dec cnt))
                                   (do (when-let [m pdm]
                                         (t/send-text token (user-id) m))
                                       pdef)))
                      :default (do (when-let [m pdm]
                                     (t/send-text token (user-id) m))
                                   pdef)
                      :throw   (throw (ex-info prm {})))
                    (let [val (try (transform raw)
                                   (catch Exception e# ::transform-error))]
                      (if (= val ::transform-error)
                        (case on-transform
                          :retry   (do
                                     (t/send-text token (user-id) trm)
                                     (if (pos? (dec cnt))
                                       (recur (dec cnt))
                                       (do (when-let [m tdm]
                                             (t/send-text token (user-id) m))
                                           tdef)))
                          :default (do (when-let [m tdm]
                                         (t/send-text token (user-id) m))
                                       tdef)
                          :throw   (throw (ex-info trm {})))

                        (let [vals (if (or (> min 1) (> max 1))
                                     (safe-split val)
                                     [val])
                              n    (count vals)]

                          (if-not (every? validate vals)
                            (case on-validate
                              :retry   (do
                                         (t/send-text token (user-id) vrm)
                                         (if (pos? (dec cnt))
                                           (recur (dec cnt))
                                           (do (when-let [m vdm]
                                                 (t/send-text token (user-id) m))
                                               vdef)))
                              :default (do (when-let [m vdm]
                                             (t/send-text token (user-id) m))
                                           vdef)
                              :throw   (throw (ex-info vrm {})))

                            (if (or (< n min) (> n max))
                              (case on-bounds
                                :retry   (do
                                           (t/send-text token (user-id) bmsg)
                                           (if (pos? (dec cnt))
                                             (recur (dec cnt))
                                             (do (when-let [m bdm]
                                                   (t/send-text token (user-id) m))
                                                 bdef)))
                                :default (do (when-let [m bdm]
                                               (t/send-text token (user-id) m))
                                             bdef)
                                :throw   (throw (ex-info bmsg {})))

                              (if (or (> min 1) (> max 1))
                                vals
                                (first vals))))))))))]

          (>! res-ch result))
        (catch ExceptionInfo e
          (if (= :receive-timeout (:type (ex-data e)))
            (>! res-ch ::receive-timeout)
            (throw e)))))

    res-ch))

(defmacro receive
  [& opts]
  (let [m# (apply hash-map opts)]
    `(let [v# (<! (run-receive*
                    (get @b/channels (user-id))
                    token
                    *timeout-chans*
                    ~m#))]
       (if (= v# ::receive-timeout)
         (throw (ex-info "receive timed out" {:type :receive-timeout}))
         v#))))

(defmacro dialog [& body]
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

(defmacro old-transmit [text]
  `(t/send-text token (user-id) ~text))

(defmacro old-receive
  ([]
   `(<! (get @b/channels (user-id))))
  ([prompt]
   `(do (old-transmit ~prompt)
        (old-receive))))
