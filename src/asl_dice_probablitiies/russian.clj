(ns asl-dice-probablitiies.russian)

(defn- add-nationality [unit]
  (assoc unit :nationality :russian))

(def first-line-squad (add-nationality {:type :squad :fp 4 :range 4 :morale 7 :class :first-line}))
(def elite-box-squad (add-nationality {:type :squad :fp 4 :range 5 :morale 8 :class :first-line}))

(def nine-minus-one-leader (add-nationality {:type :leader :morale 9 :leadership-modifier -1 :class :elite}))

(def mmg (add-nationality {:type :mmg :fp 4 :range 10 :breakdown 11}))