(ns asl-dice-probablitiies.nationalities)

(defn nationality-name [{:keys [nationality]}]
  (get {:german "German" :russian "Russian" :american "American"} nationality "Unknown"))

