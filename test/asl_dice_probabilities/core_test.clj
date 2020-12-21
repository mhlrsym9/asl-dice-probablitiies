(ns asl-dice-probabilities.core-test
  (:require [clojure.test :refer :all]
            [asl-dice-probabilities.core :refer :all]
            [asl-dice-probabilities.infantry :as infantry]
            [asl-dice-probabilities.german :as g]
            [asl-dice-probabilities.russian :as r]
            [asl-dice-probabilities.support-weapons :as sw]))

(deftest a-test
  (testing "FIXME, I fail."
    (let [attacker-location-1 {:stack (list {:possessions (list {:type :dc})
                                             :units       (list {:type :leader :morale 9 :leadership-modifier -1 :class :elite :status :unbroken :wounded? false})}
                                            {:possessions (list {:type :lmg :fp 3 :range 8 :breakdown 12 :malfunctioned? false})
                                             :units       (list {:type :squad :fp 5 :range 4 :morale 8 :class :elite :status :unbroken})})
                               :range 3}
          attacker-location-2 {:stack (list {:possessions (list)
                                             :units       (list {:type :squad :fp 4 :range 6 :morale 7 :class :first-line :status :unbroken})}
                                            {:possessions (list)
                                             :units       (list {:type :squad :fp 4 :range 6 :morale 7 :class :first-line :status :unbroken})})
                               :range 4}
          attacker-location-3 {:stack (list {:possessions (list)
                                             :units       (list {:type :squad :fp 4 :range 3 :morale 6 :class :conscript :status :unbroken})})
                               :range 4}
          fire-group-1 (list attacker-location-1)
          fire-group-2 (list attacker-location-2)
          fire-group-3 (list attacker-location-1 attacker-location-2)
          fire-group-4 (list attacker-location-1 attacker-location-2 attacker-location-3)]
      (is (= 8 (reduce calculate-attacker-location-firepower 0 fire-group-1)))
      (is (= 8 (reduce calculate-attacker-location-firepower 0 fire-group-2)))
      (is (= 16 (reduce calculate-attacker-location-firepower 0 fire-group-3)))
      (is (= -1 (total-drm-for-attacker fire-group-1)))
      (is (= 0 (total-drm-for-attacker fire-group-2)))
      (is (= 0 (total-drm-for-attacker fire-group-3)))
      (is (not (has-fire-group-cowered? fire-group-1 3 4)))
      (is (not (has-fire-group-cowered? fire-group-1 3 3)))
      (is (has-fire-group-cowered? fire-group-2 3 3))
      (is (not (can-fire-group-double-cower? fire-group-1)))
      (is (not (can-fire-group-double-cower? fire-group-2)))
      (is (not (can-fire-group-double-cower? fire-group-3)))
      (is (can-fire-group-double-cower? fire-group-4)))))

(deftest b-test
  (testing "total-drm-for-defender"
    (let [defender-location {:stack   (list {:possessions (list)
                                             :units       (list {:type :leader :morale 9 :leadership-modifier -1 :class :elite :status :unbroken :wounded? false})}
                                            {:possessions (list {:type :mmg :fp 4 :range 10 :breakdown 11 :malfunctioned? false})
                                             :units       (list {:type :squad :fp 4 :range 5 :morale 8 :class :elite :status :unbroken})})
                             :terrain :stone-building}]
      (is (= 3 (total-drm-for-defender defender-location))))))

(deftest c-test
  (testing "Unique ids"
    (let [attackers (repeatedly 5 (partial infantry/initialize g/first-line-squad))
          sws (repeatedly 6 (partial sw/initialize r/mmg))]
      (is (= "German Squad E" (:id (last attackers))))
      (is (= "Russian MMG F" (:id (last sws))))
      (is (= false (:malfunctioned? (last sws)))))))
