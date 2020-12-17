(ns asl-dice-probablitiies.utilities
  (:require [asl-dice-probablitiies.nationalities :as nationalities]))

(defn- type-name [{:keys [type]} type-names]
  (get type-names type "Unknown"))

(defn- next-letter [l]
  (if (= "Z" (last l))
    (apply str (repeat (inc (count l)) "A"))
    (apply str (repeat (count l) (char (inc (int (first l))))))))

(defn- unique-ids [{:keys [nationality]} atom default-ids]
  (if-let [ids (get @atom nationality)]
    ids
    default-ids))

(defn- unique-id [{:keys [type nationality] :as unit} atom default-ids]
  (let [ids (unique-ids unit atom default-ids)
        id (get ids type)
        next-id (next-letter id)]
    (swap! atom assoc nationality (assoc ids type next-id))
    id))

(defn build-id [unit atom type-names default-ids]
  (str (nationalities/nationality-name unit) " " (type-name unit type-names) " " (unique-id unit atom default-ids)))

