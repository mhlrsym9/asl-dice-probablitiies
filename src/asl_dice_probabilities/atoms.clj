(ns asl-dice-probabilities.atoms)

(def infantry-ids-atom (atom {}))

(def support-weapon-ids-atom (atom {}))

(defn reset-atoms []
  (reset! infantry-ids-atom {})
  (reset! support-weapon-ids-atom {}))