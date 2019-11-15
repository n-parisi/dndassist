(ns dndassist.parse.core-test
  (:require [clojure.test :refer :all]
            [dndassist.parse.core :refer :all]))

(deftest parse-json-test
  (testing "Parse test json to map"
    (is (not-empty (json-to-char-map (slurp "test/resources/test.json")))))
  )