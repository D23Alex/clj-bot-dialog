(ns clj-bot-dialog.receive-test
  (:require
    [clj-bot-dialog.lang :as l]
    [clj-bot-dialog.lang :as l]
    [clj-bot-dialog.test-utils :refer [simulate-dialog]]
    [clojure.string :as str]
    [clojure.test :refer :all]))

(deftest always-true
  (testing "always-true")
  (is (= 1 1)))

(deftest receive-basic-string
  (testing "string"
    (let [dialog
          (l/dialog
            (let [s (l/receive :prompt "Enter text:")]
              (l/transmit (str "You said: " s))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Enter text:"}
         {:from :user :text "Hello"}
         {:from :bot  :text "You said: Hello"}]))))

(deftest receive-int-transform
  (testing "int via transform"
    (let [dialog
          (l/dialog
            (let [n (l/receive
                      :prompt    "Number:"
                      :transform #(Integer/parseInt %)
                      :validate  int?
                      :error-msg "Not an int")]
              (l/transmit (str "Got: " n))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Number:"}
         {:from :user :text "foo"}
         {:from :bot  :text "Not an int"}
         {:from :bot  :text "Number:"}
         {:from :user :text "42"}
         {:from :bot  :text "Got: 42"}]))))

(deftest receive-yes-no-via-choices
  (testing "yes/no boolean"
    (let [dialog
          (l/dialog
            (let [ok? (l/receive
                        :prompt  "Confirm?"
                        :choices {"y" true "n" false})]
              (l/transmit (if ok? "OK" "Cancel"))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Confirm? (y/n)"}
         {:from :user :text "y"}
         {:from :bot  :text "OK"}]))))

(deftest receive-regex
  (testing "regex validation"
    (let [dialog
          (l/dialog
            (let [d (l/receive
                      :prompt  "Date:"
                      :pattern #"\d{4}-\d{2}-\d{2}")]
              (l/transmit d)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Date:"}
         {:from :user :text "abcd"}
         {:from :bot  :text "Неверный ввод, попробуйте снова."}
         {:from :bot  :text "Date:"}
         {:from :user :text "2025-08-15"}
         {:from :bot  :text "2025-08-15"}]))))

(deftest receive-regex-retry-default
  (testing "regex retry then default"
    (let [dialog
          (l/dialog
            (let [d (l/receive
                      :prompt          "Code:"
                      :pattern         #"[A-Z]{3}"
                      :retry           1
                      :default         "XXX"
                      :on-pattern      :default
                      :pattern-default "XXX")]
              (l/old-transmit d)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Code:"}
         {:from :user :text "yy"}
         {:from :bot  :text "Неверный ввод, попробуйте снова."}
         {:from :bot  :text "XXX"}]))))

(deftest receive-transform-trim
  (testing "trim whitespace"
    (let [dialog
          (l/dialog
            (let [s (l/receive
                      :prompt    "Name:"
                      :transform str/trim)]
              (l/transmit s)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Name:"}
         {:from :user :text "  Bob  "}
         {:from :bot  :text "Bob"}]))))

(deftest receive-choices-seq
  (testing "choices from seq"
    (let [opts ["red" "green" "blue"]
          cs   (str "(" (str/join "/" opts) ")")
          dialog
          (l/dialog
            (let [c (l/receive :prompt "Color:" :choices opts)]
              (l/transmit c)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text (str "Color: " cs)}
         {:from :user :text "green"}
         {:from :bot  :text "green"}]))))

(deftest receive-choices-map
  (testing "choices from map key→label (show keys)"
    (let [m   {"a" "Alpha" "b" "Beta"}
          cs  "(a/b)"
          dialog
          (l/dialog
            (let [k (l/receive :prompt "Pick:" :choices m)]
              (l/transmit k)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text (str "Pick: " cs)}
         {:from :user :text "b"}
         {:from :bot  :text "b"}]))))

(deftest receive-validate
  (testing "custom validate fn"
    (let [dialog
          (l/dialog
            (let [n (l/receive
                      :prompt    "Num:"
                      :transform #(Integer/parseInt %)
                      :validate  odd?
                      :error-msg "Must be odd")]
              (l/transmit (str n))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Num:"}
         {:from :user :text "4"}
         {:from :bot  :text "Must be odd"}
         {:from :bot  :text "Num:"}
         {:from :user :text "5"}
         {:from :bot  :text "5"}]))))

(deftest receive-default
  (testing "default when exhausted"
    (let [dialog
          (l/dialog
            (let [n (l/receive
                      :prompt    "X:"
                      :transform #(Integer/parseInt %)
                      :retry     1
                      :default   0)]
              (l/transmit (str n))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "X:"}
         {:from :user :text "bad"}
         {:from :bot  :text "Неверный ввод, попробуйте снова."}
         {:from :bot  :text "0"}]))))

(deftest receive-multi
  (testing "multi-select comma split"
    (let [opts ["a" "b" "c"]
          cs   "(a/b/c)"
          dialog
          (l/dialog
            (let [xs (l/receive :prompt "Tags:" :choices opts :min 1 :max 2)]
              (l/transmit (pr-str xs))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text (str "Tags: " cs)}
         {:from :user :text "a,b"}
         {:from :bot  :text "[\"a\" \"b\"]"}]))))

(deftest receive-multi-count-error
  (testing "multi-select count violation"
    (let [opts ["x" "y" "z"]
          cs   "(x/y/z)"
          dialog
          (l/dialog
            (let [xs (l/receive
                       :prompt  "Tags:"
                       :choices opts
                       :min     2
                       :max     2
                       :error-msg "Need exactly 2")]
              (l/transmit (pr-str xs))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text (str "Tags: " cs)}
         {:from :user :text "x"}
         {:from :bot  :text "Need exactly 2"}
         {:from :bot  :text (str "Tags: " cs)}
         {:from :user :text "x,z"}
         {:from :bot  :text "[\"x\" \"z\"]"}]))))

(deftest receive-pattern-default
  (testing "pattern-default returned immediately"
    (let [dialog
          (l/dialog
            (let [d (l/receive
                      :prompt           "Num:"
                      :pattern          #"\d+"
                      :on-pattern       :default
                      :pattern-default  "ZERO")]
              (l/transmit d)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Num:"}
         {:from :user :text "abc"}
         {:from :bot  :text "ZERO"}]))))

(deftest receive-transform-default
  (testing "transform-default returned on exception"
    (let [dialog
          (l/dialog
            (let [d (l/receive
                      :prompt            "P:"
                      :transform         (fn [_] (throw (Exception.)))
                      :on-transform      :default
                      :transform-default "DEF")]
              (l/transmit d)))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "P:"}
         {:from :user :text "foo"}
         {:from :bot  :text "DEF"}]))))

;; 15) on-validate default
(deftest receive-validate-default
  (testing "validate-default returned on semantic failure"
    (let [dialog
          (l/dialog
            (let [d (l/receive
                      :prompt           "Check:"
                      :transform        identity
                      :validate         (fn [_] false)
                      :on-validate      :default
                      :validate-default 123)]
              (l/transmit (str d))))]
      (simulate-dialog
        dialog
        [{:from :bot  :text "Check:"}
         {:from :user :text "anything"}
         {:from :bot  :text "123"}]))))