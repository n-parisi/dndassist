(ns dndassist.parse.core
  (:require [clojure.data.json :as json])
  (:gen-class))

(defn json-to-char-map
  "Parse a JSON character sheet from dndbeyond.com into a map"
  [char-json]
  (json/read-str char-json :key-fn keyword))