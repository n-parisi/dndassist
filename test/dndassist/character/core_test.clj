(ns dndassist.character.core-test
  (:require [clojure.test :refer :all]
            [dndassist.character.core :refer :all]
            [dndassist.parse.core :as p]))

(def test-char (p/json-to-char-map (slurp "test/resources/test.json")))

(deftest class-list-test
  (testing "Get class info from test char"
    (is (= (char-class test-char) '(["Druid" "Circle of the Land (Coast)"])))))

(deftest level-test
  (testing "Get level from test char"
    (is (= (level test-char) 3))))

(deftest stat-test
  (testing "Get stats from test char"
    (is (and
      (= (get-stat test-char :str) [8 -1])
      (= (get-stat test-char :con) [13 1])
      (= (get-stat test-char :dex) [14 2])
      (= (get-stat test-char :int) [13 1])
      (= (get-stat test-char :wis) [16 3])
      (= (get-stat test-char :cha) [12 1])
      ))))

(deftest hp-test
  (testing "Get hp from test char"
    (is (= (hp test-char) [11 21]))))