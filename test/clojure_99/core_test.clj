(ns clojure-99.core-test
  (:use [clojure.test]
        [clojure-99.core]))

(def LONG_LIST [1 2 3 4 5])
(def STRING_LIST ["Java" "User" "Group"])

(deftest problem01-test
  (is (= 5 (problem01 LONG_LIST)))
  (is (= "Group" (problem01 STRING_LIST)))
  (is (nil? (problem01 []))))

(deftest problem02-test
  (is (= 4 (problem02 LONG_LIST)))
  (is (= "User" (problem02 STRING_LIST)))
  (is (nil? (problem02 [])))
  (is (nil? (problem02 ["One Element List"]))))

(deftest problem03-test
  (is (= 1 (problem03 LONG_LIST 1)))
  (is (= 3 (problem03 LONG_LIST 3)))
  (is (= "User" (problem03 STRING_LIST 2)))
  (is (nil? (problem03 [] 1)))
  (is (nil? (problem03 ["One Element List"] 3))))

(deftest problem04-test
  (is (= 5 (problem04 LONG_LIST)))
  (is (= 3 (problem04 STRING_LIST)))
  (is (= 0 (problem04 []))))

(deftest problem05-test
  (is (= [5 4 3 2 1] (problem05 LONG_LIST)))
  (is (= ["Group" "User" "Java"] (problem05 STRING_LIST)))
  (is (= STRING_LIST (problem05 (problem05 STRING_LIST))))
  (is (= [] (problem05 []))))

(run-tests)
