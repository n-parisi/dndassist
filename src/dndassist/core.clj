(ns dndassist.core
  (:require
    [dndassist.parse.core :as p]
    [dndassist.character.core :as c])
  (:gen-class))

;Set up character map
(def character-data {:w "resources/wiskath.json"
                     :t "resources/toeni.json"
                     :m "resources/mungo.json"
                     })
(def chars-map (reduce (fn [chars-map [char-key char-json]]
                         (assoc chars-map char-key (p/json-to-char-map (slurp char-json))))
                       {}
                       character-data))

(defn all
  "Helper function to call a function on every character"
  [function & argz]
   (let [char-keys (keys chars-map)]
     (map #(let [result (if (nil? argz)
                          (function %)
                          (apply function % argz))]
             (do
              (println % "\n" result "\n")
              result))
     char-keys)
     ))

(defn spells
  "Return a map with level->[spells] where spells are names only. Optional to show prepared spells only"
  ([char-key]
   (spells char-key false))
  ([char-key prepared?]
   (let [char-map (char-key chars-map)
         spell-map (c/spells-for-level char-map)]
     (c/spell-names (if prepared?
                      (c/prepared-spells spell-map)
                      spell-map)))
   ))

(defn char-class
  "Return character class and subclass info"
  [char-key]
  (c/char-class (char-key chars-map)))

(defn hp
  "Return [current-hp max-hp] for character"
  [char-key]
  (c/hp (char-key chars-map)))

(defn stat
  "Return [stat mod]"
  [char-key stat]
  (c/get-stat (char-key chars-map) stat))

(defn level
  "Return total level for character"
  [char-key]
  (c/level (char-key chars-map)))

(defn ac
  "Return AC for character"
  [char-key]
  (c/ac (char-key chars-map)))

(defn actions
  "Return list of actions"
  ([char-key]
   (actions char-key :n))
  ([char-key option]
  (let [action-list (c/actions (char-key chars-map))]
    (cond
      (= option :n) (c/action-names action-list)
      (= option :d) (c/action-descriptions action-list)
      :else action-list))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
