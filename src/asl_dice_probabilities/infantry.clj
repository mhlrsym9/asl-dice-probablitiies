(ns asl-dice-probabilities.infantry
  (:require [asl-dice-probabilities.nationalities :as nationalities]
            [asl-dice-probabilities.utilities :as utils]
            [asl-dice-probabilities.atoms :as atoms]))

(def infantry-ids {:squad "A" :half-squad "A" :vehicular-crew "A" :crew "A" :leader "A" :hero "A"})

(def type-names {:squad "Squad" :half-squad "Half Squad" :vehicular-crew "Vehicular Crew" :crew "Crew" :leader "Leader" :hero "Hero"})

(defn is-leader? [{:keys [type]}]
  (= :leader type))

(defn is-hero? [{:keys [type]}]
  (= :hero type))

(defn is-smc? [counter]
  (or (is-leader? counter)
      (is-hero? counter)))

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

(defn is-good-order? [{:keys [status in-melee?]}]
  (and (= false in-melee?)
       (or (= :unbroken status)
           (= :pinned status))))

(defn is-unpinned? [{:keys [status]}]
  (not= :pinned status))

(defn- build-id [unit]
  (utils/build-id unit atoms/infantry-ids-atom type-names infantry-ids))

(defn initialize [unit]
  (assoc (if (is-smc? unit) (assoc unit :wounded? false) unit)
    :id (build-id unit)
    :status :unbroken
    :in-melee? false))