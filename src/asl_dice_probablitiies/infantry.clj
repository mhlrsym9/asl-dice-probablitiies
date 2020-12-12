(ns asl-dice-probablitiies.infantry)

(def infantry-ids {:squad "A" :half-squad "A" :vehicular-crew "A" :crew "A" :leader "A" :hero "A"})

(def infantry-ids-atom (atom {}))

(defn nationality-name [{:keys [nationality]}]
  (get {:german "German" :russian "Russian" :american "American"} nationality "Unknown"))

(defn type-name [{:keys [type]}]
  (get {:squad "Squad" :half-squad "Half Squad" :vehicular-crew "Vehicular Crew" :crew "Crew" :leader "Leader" :hero "Hero"} type "Unknown"))

(defn next-letter [l]
  (if (= "Z" (last l))
    (apply str (repeat (inc (count l)) "A"))
    (apply str (repeat (count l) (char (inc (int (first l))))))))

(defn unique-ids [{:keys [nationality]}]
  (if-let [ids (get @infantry-ids-atom nationality)]
    ids
    infantry-ids))

(defn unique-id [{:keys [type nationality] :as unit}]
  (let [ids (unique-ids unit)
        id (get ids type)
        next-id (next-letter id)]
    (swap! infantry-ids-atom assoc nationality (assoc ids type next-id))
    id))

(defn build-id [unit]
  (str (nationality-name unit) " " (type-name unit) " " (unique-id unit)))

(defn initialize [{:keys [type] :as unit}]
  (assoc unit :id (build-id unit)
              :status :unbroken))