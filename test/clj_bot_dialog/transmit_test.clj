(ns clj-bot-dialog.transmit-test
  (:require
    [clj-bot-dialog.helpers :as h]
    [clj-bot-dialog.lang :as l]
    [clj-bot-dialog.lang :as l]
    [clj-bot-dialog.test-utils :refer [simulate-dialog]]
    [clojure.test :refer :all]))

(deftest always-true
  (testing "always-true")
  (is (= 1 1)))

(deftest transmit-single
  (testing "transmit sends a single message"
    (let [dialog
          (l/dialog
            (l/old-transmit "Hello"))]
      (simulate-dialog
        dialog
        [{:from :bot :text "Hello"}]))))

(l/dialog
  (l/transmit
    "One"
    "Two"
    "Three"))


(deftest transmit-multiple-literals
  (testing "transmit sends each literal string separately"
    (let [dialog
          (l/dialog
            (l/transmit
              "One"
              "Two"
              "Three"))]
      (simulate-dialog
        dialog
        [{:from :bot :text "One"}
         {:from :bot :text "Two"}
         {:from :bot :text "Three"}]))))


(deftest transmit-dynamic-args
  (testing "transmit with values from locals"
    (let [dialog
          (l/dialog
            (let [a (str "Line-" 1)
                  b (str "Line-" 2)]
              (l/transmit a b)))]
      (simulate-dialog
        dialog
        [{:from :bot :text "Line-1"}
         {:from :bot :text "Line-2"}]))))


(deftest transmit-chain
  (testing "back-to-back transmit calls preserve order"
    (let [dialog
          (l/dialog
            (l/old-transmit "A")
            (l/transmit "B" "C")
            (l/old-transmit "D"))]
      (simulate-dialog
        dialog
        [{:from :bot :text "A"}
         {:from :bot :text "B"}
         {:from :bot :text "C"}
         {:from :bot :text "D"}]))))


(deftest transmit-in-plet
  (testing "transmit inside a plet with both static and computed texts"
    (let [dialog
          (l/dialog
            (h/plet [x "foo"
                     y (str x "-bar")]
                    (l/transmit x y)))]
      (simulate-dialog
        dialog
        [{:from :bot :text "foo"}
         {:from :bot :text "foo-bar"}]))))