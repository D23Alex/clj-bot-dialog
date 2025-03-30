(ns clj-bot-dialog.helpers
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [codax.core :as codax])
  (:import (java.lang ProcessHandle)
           (java.util UUID)))

(def base-dir
  (or (System/getenv "XDG_RUNTIME_DIR") "/tmp"))

(defn get-db-path []
  (let [pid (.pid (ProcessHandle/current))
        tid (.getId (Thread/currentThread))
        db-path (str base-dir "/persistent-let-" pid "-" tid ".db")]
    db-path))

(def ^:dynamic *go-thread-id* nil)

(defonce temp-dbs (atom #{}))

(defonce memory-storage (atom {}))

(defn serializable-to-edn? [v]
  (try
    (let [s (pr-str v)]
      (edn/read-string s)
      true)
    (catch Exception _ false)))

(defn save-value [db sym val]
  (if (serializable-to-edn? val)
    (codax/with-write-transaction [db tx]
                                  (codax/assoc-at tx [sym] val))
    (swap! memory-storage assoc sym val)))

(defn load-value [db sym]
  (if-let [val (get @memory-storage sym)]
    val
    (codax/with-read-transaction [db tx]
                                 (let [res (codax/get-at tx [sym])]
                                   (if (= res ::codax/not-found) nil res)))))

(defn delete-db [db-path]
  (when (.exists (io/file db-path))
    (.delete (io/file db-path))))

(.addShutdownHook (Runtime/getRuntime)
                  (Thread.
                    (fn []
                      (doseq [db @temp-dbs]
                        (delete-db db)))))

(defmacro plet [bindings & body]
  "Persistent let: like 'let', but persistent"
  (let [db-path (get-db-path)
        db# (gensym "db-")
        pairs (partition 2 bindings)
        syms (map first pairs)]
    `(binding [*go-thread-id* (or *go-thread-id* (str "go-" (UUID/randomUUID)))]
       (let [~db# (codax/open-database! ~db-path)]
         (try
           (swap! temp-dbs conj ~db-path)
           (let [~@(mapcat (fn [[s v]] `[~s (or (load-value ~db# '~s) ~v)])
                           pairs)]
             ~@(map (fn [s] `(save-value ~db# '~s ~s)) syms)
             ~@body)
           (finally
             (codax/close-database! ~db#)
             (delete-db ~db-path)
             (swap! temp-dbs disj ~db-path)))))))
