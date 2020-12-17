(ns asl-dice-probablitiies.infantry
  (:require [asl-dice-probablitiies.nationalities :as nationalities]
            [asl-dice-probablitiies.utilities :as utils]))

(def infantry-ids {:squad "A" :half-squad "A" :vehicular-crew "A" :crew "A" :leader "A" :hero "A"})

(def infantry-ids-atom (atom {}))

(def type-names {:squad "Squad" :half-squad "Half Squad" :vehicular-crew "Vehicular Crew" :crew "Crew" :leader "Leader" :hero "Hero"})

(defn is-leader? [{:keys [type]}]
  (= :leader type))

(defn is-hero? [{:keys [type]}]
  (= :hero type))

(defn is-smc? [unit]
  (or (is-leader? unit)
      (is-hero? unit)))

(defn is-squad? [{:keys [type]}]
  (= :squad type))

(defn is-half-squad? [{:keys [type]}]
  (= :half-squad type))

(defn is-mmc? [counter]
  (or (is-squad? counter)
      (is-half-squad? counter)))

(defn is-infantry? [counter]
  (or (is-smc? counter)
      (is-mmc? counter)))

(defn- build-id [unit]
  (utils/build-id unit infantry-ids-atom type-names infantry-ids))

(defn initialize [unit]
  (assoc (if (is-smc? unit) (assoc unit :wounded? false) unit)
    :id (build-id unit)
    :status :unbroken))