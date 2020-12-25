(ns asl-dice-probabilities.support-weapons
  (:require [asl-dice-probabilities.utilities :as utils]
            [asl-dice-probabilities.atoms :as atoms]))

(def support-weapon-ids {:lmg "A" :mmg "A" :hmg "A" :dc "A"})

(def type-names {:lmg "LMG" :mmg "MMG" :hmg "HMG" :dc "DC"})

(def dc {:type :dc})

(defn- build-id [unit]
  (utils/build-id unit atoms/support-weapon-ids-atom type-names support-weapon-ids))

(defn is-mg? [{:keys [type]}]
  (#{:lmg :mmg :hmg} type))

(defn is-dc? [{:keys [type]}]
  (= :dc type))

(defn is-flamethrower? [{:keys [type]}]
  (= :flamethrower type))

(defn is-atr? [{:keys [type]}]
  (= :atr type))

(defn is-bazooka? [{:keys [type]}]
  (= :bazooka type))

(defn is-psk? [{:keys [type]}]
  (= :psk type))

(defn is-light-anti-tank-weapon? [counter]
  (or (is-atr? counter)
      (is-bazooka? counter)
      (is-psk? counter)))

(defn is-light-mortar? [{:keys [type]}]
  (= :light-mortar type))

(defn is-sw? [counter]
  (or (is-mg? counter)
      (is-dc? counter)
      (is-flamethrower? counter)
      (is-light-anti-tank-weapon? counter)
      (is-light-mortar? counter)))

(defn initialize [unit]
  (assoc unit :id (build-id unit) :malfunctioned? false))
