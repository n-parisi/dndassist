(ns dndassist.character.core
  (:gen-class))

;CONSTANTS
(def stat-ids {:str 1
               :dex 2
               :con 3
               :int 4
               :wis 5
               :cha 6})

;CLASS
(defn class-list
  "JSON list of all classes - helper function"
  [char-map]
  (get-in char-map [:character :classes]))

(defn char-class
  "Return list of vectors each containing class/subclass combination"
  [char-map]
  (let [classes (class-list char-map)]
    (map #(vector (get-in % [:definition :name]) (get-in % [:subclassDefinition :name]))
         classes)
    ))

;ATTRIBUTES
(defn level
  "Return total level"
  [char-map]
  (let [classes (class-list char-map)
        levels (map :level classes)]
    (reduce + levels)
    ))

(defn get-stat
  "Return vector of [stat modifier]"
  [char-map stat]
  (let [pc (:character char-map)
        stat-id (stat stat-ids)
        base-attr (->>
                    (:stats pc)
                    (filter #(= stat-id (:id %)))
                    first :value)
        ;helper functions for filtering/suming lists of modifiers
        mod-filter #(and (= (:type %) "bonus") (= (:entityId %) stat-id))
        mod-category-score #(reduce (fn [cat-score mod] (+ cat-score (:value mod)))
                                    0 (filter mod-filter %))
        ;total bonus modifier from all modifiers
        bonus-attr (reduce (fn [mod-score [mod-category mods]]
                             (+ mod-score (mod-category-score mods)))
                           0 (:modifiers pc))
        total-attr (+ base-attr bonus-attr)
        stat-mod (-> total-attr (- 10) (/ 2) int)]
    [total-attr stat-mod]
    ))

(defn hp
  "Returns HP snapshot [current-hp max-hp"
  [char-map]
  (let [pc (:character char-map)
        base-hp (:baseHitPoints pc)
        bonus-hp (* (second (get-stat char-map :con)) (level char-map))
        max-hp (+ base-hp bonus-hp)
        lost-hp (:removedHitPoints pc)]
    [(- max-hp lost-hp) max-hp]))

;ac needs a test
(defn armor-bonus
  [char-map armor-def]
  "Helper function to get AC bonus for char and armorType"
  (let [armorTypeId (:armorTypeId armor-def)
        dex-bonus (second (get-stat char-map :dex))]
    (cond
      (= armorTypeId 1) dex-bonus
      (= armorTypeId 2) (if (> dex-bonus 2) 2 dex-bonus)
      :else 0)
    ))

(defn ac
  "Returns character AC"
  [char-map]
  (let [pc (:character char-map)
        inventory (:inventory pc)
        armor-filter #(= (get-in % [:definition :filterType]) "Armor")
        equipped-filter #(:equipped %)
        all-equipped-armor (->> inventory (filter armor-filter) (filter equipped-filter))
        ]
    (reduce (fn [ac-score armor] (let [armor-def (:definition armor)]
                                   (+ ac-score (:armorClass armor-def) (armor-bonus char-map armor-def))))
            0 all-equipped-armor)
    ))

;ACTIONS
;actions need tests :(
(defn actions
  "Return list of actions character can take"
  [char-map]
  (let [actions (get-in char-map [:character :actions])]
    (reduce (fn [action-list [action-cat cat-actions]] (concat action-list cat-actions)) [] actions)
    ))

(defn action-names
  "Turn list of actions into names only"
  [action-list]
  (map :name action-list)
  )

;Could use an HTML tag stripper for :description
(defn action-descriptions
  "Turn lsit of actions in name - description pairs"
  [action-list]
  (map #(vector (:name %) (:description %)) action-list))

;PASSIVE SKILLS
;;PROCIFIENCIES

;SPELLS
;spell slots - have to compare two diff data sets

;spells need tests :(
(defn spells-for-level
  "Return map of all known spells grouped by level"
  [char-map]
  (let [spells-for-class (get-in char-map [:character :classSpells])
        spells-all-classes (reduce (fn [spells-all-classes class-spells] (concat spells-all-classes (:spells class-spells))) [] spells-for-class)
        other-spells (get-in char-map [:character :spells :class])
        all-spells (concat spells-all-classes other-spells)]
    (reduce (fn [spell-map spell] (let [level (get-in spell [:definition :level])]
                                    (if (contains? spell-map level)
                                      (assoc spell-map level (conj (get spell-map level) spell))
                                      (assoc spell-map level [spell])
                                      )))
            {}
            all-spells)))

;functions to apply to an individual spell
(defn spell-name
  "Apply to a spell and return its name"
  [spell]
  (get-in spell [:definition :name]))

(defn prepared?
  "Return true if spell is prepared or a cantrip"
  [spell]
  (or (:prepared spell) (:alwaysPrepared spell) (= (get-in spell [:definition :level]) 0)))

;functions to apply to a spell map like { level : [list, of, spells] }
(defn spell-names
  "Apply to a map of spells to return just their names"
  [spell-map]
  (reduce (fn [altered-map [level spells]] (assoc altered-map level (map spell-name spells))) {} spell-map))

(defn prepared-spells
  "Apply to a map of spells to filter out spells that aren't prepared"
  [spell-map]
  (reduce (fn [altered-map [level spells]] (assoc altered-map level (filter prepared? spells))) {} spell-map))