(ns asl-dice-probablitiies.support-weapons
  (:require [asl-dice-probablitiies.utilities :as utils]))

(def support-weapon-ids {:lmg "A" :mmg "A" :hmg "A" :dc "A"})

(def type-names {:lmg "LMG" :mmg "MMG" :hmg "HMG" :dc "DC"})

(def support-weapon-ids-atom (atom {}))

(defn- build-id [unit]
  (utils/build-id unit support-weapon-ids-atom type-names support-weapon-ids))

(defn initialize [unit]
  (assoc unit :id (build-id unit) :malfunctioned? false))
