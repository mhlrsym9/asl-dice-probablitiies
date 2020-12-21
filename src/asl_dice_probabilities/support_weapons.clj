(ns asl-dice-probabilities.support-weapons
  (:require [asl-dice-probabilities.utilities :as utils]))

(def support-weapon-ids {:lmg "A" :mmg "A" :hmg "A" :dc "A"})

(def type-names {:lmg "LMG" :mmg "MMG" :hmg "HMG" :dc "DC"})

(def support-weapon-ids-atom (atom {}))

(def dc {:type :dc})

(defn- build-id [unit]
  (utils/build-id unit support-weapon-ids-atom type-names support-weapon-ids))

(defn is-mg? [{:keys [type]}]
  (some #{:lmg :mmg :hmg} type))

(defn- is-dc? [{:keys [type]}]
  (= :dc type))

(defn is-sw? [counter]
  (or (is-mg? counter)
      (is-dc? counter)))

(defn initialize [unit]
  (assoc unit :id (build-id unit) :malfunctioned? false))
