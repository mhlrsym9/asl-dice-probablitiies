(ns asl-dice-probabilities.core
  (:require [asl-dice-probabilities.german :as g]
            [asl-dice-probabilities.russian :as r]
            [asl-dice-probabilities.infantry :as infantry]
            [asl-dice-probabilities.support-weapons :as sw])
  (:gen-class))

(def d6 (range 1 7))

(def base-morale 8)

(defn fails-two-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) (> (+ morale-check-roll 1) morale)
          (= final-dr 4) (> (+ morale-check-roll 1) morale)
          (= final-dr 5) (> morale-check-roll morale)
          :else false)))

(defn fails-four-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) (> (+ morale-check-roll 2) morale)
          (= final-dr 4) (> (+ morale-check-roll 1) morale)
          (= final-dr 5) (> (+ morale-check-roll 1) morale)
          (= final-dr 6) (> morale-check-roll morale)
          :else false)))

(defn fails-six-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) (> (+ morale-check-roll 2) morale)
          (= final-dr 5) (> (+ morale-check-roll 1) morale)
          (= final-dr 6) (> (+ morale-check-roll 1) morale)
          (= final-dr 7) (> morale-check-roll morale)
          :else false)))

(defn fails-eight-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) (> (+ morale-check-roll 2) morale)
          (= final-dr 5) (> (+ morale-check-roll 2) morale)
          (= final-dr 6) (> (+ morale-check-roll 1) morale)
          (= final-dr 7) (> (+ morale-check-roll 1) morale)
          (= final-dr 8) (> morale-check-roll morale)
          :else false)))

(defn fails-twelve-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) (> (+ morale-check-roll 3) morale)
          (= final-dr 5) (> (+ morale-check-roll 2) morale)
          (= final-dr 6) (> (+ morale-check-roll 2) morale)
          (= final-dr 7) (> (+ morale-check-roll 1) morale)
          (= final-dr 8) (> (+ morale-check-roll 1) morale)
          (= final-dr 9) (> morale-check-roll morale)
          :else false)))

(defn fails-sixteen-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) true
          (= final-dr 5) (> (+ morale-check-roll 3) morale)
          (= final-dr 6) (> (+ morale-check-roll 2) morale)
          (= final-dr 7) (> (+ morale-check-roll 2) morale)
          (= final-dr 8) (> (+ morale-check-roll 1) morale)
          (= final-dr 9) (> (+ morale-check-roll 1) morale)
          (= final-dr 10) (> morale-check-roll morale)
          :else false)))

(defn fails-twenty-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) true
          (= final-dr 5) (> (+ morale-check-roll 4) morale)
          (= final-dr 6) (> (+ morale-check-roll 3) morale)
          (= final-dr 7) (> (+ morale-check-roll 2) morale)
          (= final-dr 8) (> (+ morale-check-roll 2) morale)
          (= final-dr 9) (> (+ morale-check-roll 1) morale)
          (= final-dr 10) (> (+ morale-check-roll 1) morale)
          (= final-dr 11) (> morale-check-roll morale)
          :else false)))

(defn fails-twenty-four-fp? [attack-roll morale-check-roll dm morale]
  (let [final-dr (+ attack-roll dm)]
    (cond (< final-dr 0) true
          (= final-dr 1) true
          (= final-dr 2) true
          (= final-dr 3) true
          (= final-dr 4) true
          (= final-dr 5) true
          (= final-dr 6) (> (+ morale-check-roll 4) morale)
          (= final-dr 7) (> (+ morale-check-roll 3) morale)
          (= final-dr 8) (> (+ morale-check-roll 2) morale)
          (= final-dr 9) (> (+ morale-check-roll 2) morale)
          (= final-dr 10) (> (+ morale-check-roll 1) morale)
          (= final-dr 11) (> (+ morale-check-roll 1) morale)
          (= final-dr 12) (> morale-check-roll morale)
          :else false)))

(defn- has-cowered? [cd1 wd1 is-leader-directed?]
  (and (= cd1 wd1) (not is-leader-directed?)))

(defn does-unit-break? [cd1 wd1 cd2 wd2 fp dm morale is-leader-directed?]
  (let [attack-roll (+ cd1 wd1)
        morale-check-roll (+ cd2 wd2)
        cowered? (has-cowered? cd1 wd1 is-leader-directed?)]
    (cond (= fp 4) (if cowered?
                     (fails-two-fp? attack-roll morale-check-roll dm morale)
                     (fails-four-fp? attack-roll morale-check-roll dm morale))
          (= fp 6) (if cowered?
                     (fails-four-fp? attack-roll morale-check-roll dm morale)
                     (fails-six-fp? attack-roll morale-check-roll dm morale))
          (= fp 8) (if cowered?
                     (fails-six-fp? attack-roll morale-check-roll dm morale)
                     (fails-eight-fp? attack-roll morale-check-roll dm morale))
          (= fp 12) (if cowered?
                     (fails-eight-fp? attack-roll morale-check-roll dm morale)
                     (fails-twelve-fp? attack-roll morale-check-roll dm morale))
          (= fp 16) (if cowered?
                     (fails-twelve-fp? attack-roll morale-check-roll dm morale)
                     (fails-sixteen-fp? attack-roll morale-check-roll dm morale))
          (= fp 20) (if cowered?
                      (fails-sixteen-fp? attack-roll morale-check-roll dm morale)
                      (fails-twenty-fp? attack-roll morale-check-roll dm morale))
          (= fp 24) (if cowered?
                      (fails-twenty-fp? attack-roll morale-check-roll dm morale)
                      (fails-twenty-four-fp? attack-roll morale-check-roll dm morale)))))

(defn- is-leader-in-units? [{:keys [units]}]
  (seq (filter infantry/is-leader? units)))

(defn- does-location-have-leader? [{:keys [stack]}]
  (seq (filter is-leader-in-units? stack)))

(defn has-fire-group-cowered? [fire-group cd wd]
  (if (= cd wd)
    (not-every? does-location-have-leader? fire-group)
    false))

(defn- is-infantry-in-units? [{:keys [units]}]
  (seq (filter infantry/is-infantry? units)))

(defn- does-location-have-inexperienced-infantry? [{:keys [stack]}]
  (let [units (mapcat :units (filter is-infantry-in-units? stack))]
    (or (seq (filter infantry/is-conscript? units))
        (and (seq (filter infantry/is-green? units))
             (empty? (filter infantry/is-good-order-leader? units))))))

(defn can-fire-group-double-cower? [fire-group]
  (seq (filter does-location-have-inexperienced-infantry? fire-group)))

(defn- adjust-firepower-for-range [range-to-defender {:keys [fp range]}]
  (cond (> range-to-defender (* 2 range)) 0
        (> range-to-defender range) (/ fp 2)
        :else fp))

(defn- calculate-counter-firepower [range-to-defender fp counter]
  (let [calculate-fp (partial adjust-firepower-for-range range-to-defender)]
    (cond (infantry/is-leader? counter) fp
          (sw/is-mg? counter) (+ fp (calculate-fp counter))
          (infantry/is-mmc? counter) (+ fp (calculate-fp counter))
          :else fp)))

(defn- calculate-firepower-of-stack-in-attacker-location [range-to-defender fp {:keys [units possessions]}]
  (let [units-fp (reduce (partial calculate-counter-firepower range-to-defender) 0 units)
        possessions-fp (reduce (partial calculate-counter-firepower range-to-defender) 0 possessions)]
    (if (and (infantry/is-leader? (first units))
             (> (count units) 1))
      (+ fp units-fp (/ possessions-fp 2))
      (+ fp units-fp possessions-fp))))

(defn calculate-firepower-of-attacker-location-in-fire-group [fp {:keys [stack range]}]
  (reduce (partial calculate-firepower-of-stack-in-attacker-location range) fp stack))

(defn- extract-leadership-drm [{:keys [stack]}]
  (apply min (map :leadership-modifier (filter infantry/is-leader? (mapcat :units (filter is-leader-in-units? stack))))))

(defn- calculate-leadership-drm [fire-group]
  (if (not-every? does-location-have-leader? fire-group)
    0
    (apply max (map extract-leadership-drm fire-group))))

(defn total-drm-for-attacker [fire-group]
  (let [leadership-drm (calculate-leadership-drm fire-group)]
    leadership-drm))

(defn- calculate-terrain-drm [{:keys [terrain]}]
  (cond (= :stone-building terrain) 3
        (= :wooden-building terrain) 2
        :else 0))

(defn total-drm-for-defender [defender-location]
  (let [terrain-drm (calculate-terrain-drm defender-location)]
    terrain-drm))

(defn- adjust-fire-power-two-columns-to-the-left [total-firepower]
  (cond (>= total-firepower 36) 24
        (>= total-firepower 30) 20
        (>= total-firepower 24) 16
        (>= total-firepower 20) 12
        (>= total-firepower 16) 8
        (>= total-firepower 12) 6
        (>= total-firepower 8) 4
        (>= total-firepower 6) 2
        (>= total-firepower 4) 1
        :else 0))

(defn- adjust-fire-power-one-column-to-the-left [total-firepower]
  (cond (>= total-firepower 36) 30
        (>= total-firepower 30) 24
        (>= total-firepower 24) 20
        (>= total-firepower 20) 16
        (>= total-firepower 16) 12
        (>= total-firepower 12) 8
        (>= total-firepower 8) 6
        (>= total-firepower 6) 4
        (>= total-firepower 4) 2
        (>= total-firepower 2) 1
        :else 0))

(defn- process-kia [number defender-location])

(defn- process-7-kia [defender-location]
  (process-kia 7 defender-location))

(defn- process-6-kia [defender-location]
  (process-kia 6 defender-location))

(defn- process-5-kia [defender-location]
  (process-kia 5 defender-location))

(defn- process-4-kia [defender-location]
  (process-kia 4 defender-location))

(defn- process-3-kia [defender-location]
  (process-kia 3 defender-location))

(defn- process-2-kia [defender-location]
  (process-kia 2 defender-location))

(defn- process-1-kia [defender-location]
  (process-kia 1 defender-location))

(defn- process-k [number defender-location])

(defn- process-k-slash-4 [defender-location]
  (process-k 4 defender-location))

(defn- process-k-slash-3 [defender-location]
  (process-k 3 defender-location))

(defn- process-k-slash-2 [defender-location]
  (process-k 2 defender-location))

(defn- process-k-slash-1 [defender-location]
  (process-k 1 defender-location))

(defn- sort-leaders [l1 l2]
  (let [i1 [(:morale l2) (:leadership-modifier l1) (:id l1)]
        i2 [(:morale l1) (:leadership-modifier l2) (:id l2)]]
    (compare i1 i2)))

(defn- find-leadership-drm [id stack]
  (let [units (flatten (mapcat :units stack))
        leaders (sort sort-leaders (filter infantry/is-unpinned?
                                           (filter infantry/is-good-order?
                                                   (filter infantry/is-leader? units))))]
    (if (empty? leaders)
      0
      (if (= id (:id (first leaders)))
        0
        (:leadership-modifier (first leaders))))))

(defn- process-break-for-unit [])

(defn- process-break-for-unit-with-id-in-units [the-id units]
  (map #(let [{:keys [id]} %] (if (= id the-id) (assoc % :status :broken) %)) units))

(defn- process-break-for-unit-with-id-in-stack [id stack]
  (map #(assoc % :units (process-break-for-unit-with-id-in-units id (:units %))) stack))

(defn- check-for-break [number {:keys [morale id]} stacks]
  (for [cd d6 wd d6 s stacks]
    (let [leadership-drm (find-leadership-drm id s)]
      (if (>= morale (+ cd wd leadership-drm number))
        s
        (process-break-for-unit-with-id-in-stack id s)))))

(defn- check-for-wound-elimination [leader stacks]
  stacks)

(defn- process-leader-loss-morale-check [leader stacks]
  stacks)

(defn- process-leader-loss-task-check [leader stacks]
  stacks)

(defn- process-mc [number {:keys [stack]}]
  (let [units (flatten (mapcat :units stack))
        leaders (sort sort-leaders (filter infantry/is-leader? units))
        mmcs (filter infantry/is-mmc? units)]
    (loop [leaders leaders wounded-leaders (list) eliminated-leaders (list) broken-leaders (list) mmcs mmcs stacks (list stack)]
      (if (seq leaders)
        (let [l (first leaders)
              new-stacks (check-for-break number l stacks)]
          (recur (rest leaders) wounded-leaders eliminated-leaders broken-leaders mmcs new-stacks))
        (if (seq wounded-leaders)
          (let [l (first wounded-leaders)
                new-stacks (check-for-wound-elimination l stacks)]
            (recur leaders (rest wounded-leaders) eliminated-leaders broken-leaders mmcs new-stacks))
          (if (seq mmcs)
            (let [mmc (first mmcs)
                  new-stacks (check-for-break number mmc stacks)]
              (recur leaders wounded-leaders eliminated-leaders broken-leaders (rest mmcs) new-stacks))
            (if (seq eliminated-leaders)
              (let [l (first eliminated-leaders)
                    new-stacks (process-leader-loss-morale-check l stacks)]
                (recur leaders wounded-leaders (rest eliminated-leaders) broken-leaders mmcs new-stacks))
              (if (seq broken-leaders)
                (let [l (first broken-leaders)
                      new-stacks (process-leader-loss-task-check l stacks)]
                  (recur leaders wounded-leaders eliminated-leaders (rest broken-leaders) mmcs new-stacks))
                stacks))))))))

(defn- process-4-mc [defender-location]
  (process-mc 4 defender-location))

(defn- process-3-mc [defender-location]
  (process-mc 3 defender-location))

(defn- process-2-mc [defender-location]
  (process-mc 2 defender-location))

(defn- process-1-mc [defender-location]
  (process-mc 1 defender-location))

(defn- process-nmc [defender-location]
  (process-mc 0 defender-location))

(defn- update-status-to-pinned-in-units [the-id units]
  (map #(let [{:keys [id]} %] (if (= id the-id) (assoc % :status :pinned) %)) units))

(defn- update-status-to-pinned-in-stack [id stack]
  (map #(assoc % :units (update-status-to-pinned-in-units id (:units %))) stack))

(defn- check-for-pin [{:keys [morale id]} stacks]
  (for [cd d6 wd d6 s stacks]
    (let [leadership-drm (find-leadership-drm id s)]
      (if (>= morale (+ cd wd leadership-drm))
        s
        (update-status-to-pinned-in-stack id s)))))

(defn process-ptc [{:keys [stack]}]
  (let [units (flatten (mapcat :units stack))
        leaders (sort sort-leaders (filter infantry/is-unpinned?
                                           (filter infantry/is-good-order?
                                                   (filter #(= :leader (:type %)) units))))
        mmcs (filter infantry/is-unpinned?
                     (filter infantry/is-good-order?
                             (filter infantry/is-mmc? units)))]
    (loop [leaders leaders mmcs mmcs stacks (list stack)]
      (if (and (empty? leaders) (empty? mmcs))
        stacks
        (if (empty? leaders)
          (let [mmc (first mmcs)
                new-stacks (check-for-pin mmc stacks)]
            (recur leaders (rest mmcs) new-stacks))
          (let [l (first leaders)
                new-stacks (check-for-pin l stacks)]
            (recur (rest leaders) mmcs new-stacks)))))))

(defn- process-no-result [defender-location]
  defender-location)

(defn- process-36-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-7-kia
                         (= modified-attack-roll 1) process-6-kia
                         (= modified-attack-roll 2) process-5-kia
                         (= modified-attack-roll 3) process-4-kia
                         (= modified-attack-roll 4) process-3-kia
                         (= modified-attack-roll 5) process-2-kia
                         (= modified-attack-roll 6) process-1-kia
                         (= modified-attack-roll 7) process-k-slash-4
                         (= modified-attack-roll 8) process-4-mc
                         (= modified-attack-roll 9) process-3-mc
                         (= modified-attack-roll 10) process-2-mc
                         (= modified-attack-roll 11) process-2-mc
                         (= modified-attack-roll 12) process-1-mc
                         (= modified-attack-roll 13) process-1-mc
                         (= modified-attack-roll 14) process-nmc
                         (>= modified-attack-roll 15) process-ptc)]
    (result-fnc defender-location)))

(defn- process-30-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-6-kia
                         (= modified-attack-roll 1) process-5-kia
                         (= modified-attack-roll 2) process-4-kia
                         (= modified-attack-roll 3) process-3-kia
                         (= modified-attack-roll 4) process-2-kia
                         (= modified-attack-roll 5) process-1-kia
                         (= modified-attack-roll 6) process-k-slash-4
                         (= modified-attack-roll 7) process-4-mc
                         (= modified-attack-roll 8) process-3-mc
                         (= modified-attack-roll 9) process-2-mc
                         (= modified-attack-roll 10) process-2-mc
                         (= modified-attack-roll 11) process-1-mc
                         (= modified-attack-roll 12) process-1-mc
                         (= modified-attack-roll 13) process-nmc
                         (= modified-attack-roll 14) process-ptc
                         (>= modified-attack-roll 15) process-no-result)]
    (result-fnc defender-location)))

(defn- process-24-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-5-kia
                         (= modified-attack-roll 1) process-4-kia
                         (= modified-attack-roll 2) process-3-kia
                         (= modified-attack-roll 3) process-2-kia
                         (= modified-attack-roll 4) process-1-kia
                         (= modified-attack-roll 5) process-k-slash-4
                         (= modified-attack-roll 6) process-4-mc
                         (= modified-attack-roll 7) process-3-mc
                         (= modified-attack-roll 8) process-2-mc
                         (= modified-attack-roll 9) process-2-mc
                         (= modified-attack-roll 10) process-1-mc
                         (= modified-attack-roll 11) process-1-mc
                         (= modified-attack-roll 12) process-nmc
                         (= modified-attack-roll 13) process-ptc
                         (>= modified-attack-roll 14) process-no-result)]
    (result-fnc defender-location)))

(defn- process-20-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-4-kia
                         (= modified-attack-roll 1) process-3-kia
                         (= modified-attack-roll 2) process-2-kia
                         (= modified-attack-roll 3) process-1-kia
                         (= modified-attack-roll 4) process-k-slash-4
                         (= modified-attack-roll 5) process-4-mc
                         (= modified-attack-roll 6) process-3-mc
                         (= modified-attack-roll 7) process-2-mc
                         (= modified-attack-roll 8) process-2-mc
                         (= modified-attack-roll 9) process-1-mc
                         (= modified-attack-roll 10) process-1-mc
                         (= modified-attack-roll 11) process-nmc
                         (= modified-attack-roll 12) process-ptc
                         (>= modified-attack-roll 13) process-no-result)]
    (result-fnc defender-location)))

(defn- process-16-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-4-kia
                         (= modified-attack-roll 1) process-3-kia
                         (= modified-attack-roll 2) process-2-kia
                         (= modified-attack-roll 3) process-1-kia
                         (= modified-attack-roll 4) process-k-slash-3
                         (= modified-attack-roll 5) process-3-mc
                         (= modified-attack-roll 6) process-2-mc
                         (= modified-attack-roll 7) process-2-mc
                         (= modified-attack-roll 8) process-1-mc
                         (= modified-attack-roll 9) process-1-mc
                         (= modified-attack-roll 10) process-nmc
                         (= modified-attack-roll 11) process-ptc
                         (>= modified-attack-roll 12) process-no-result)]
    (result-fnc defender-location)))

(defn- process-12-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-3-kia
                         (= modified-attack-roll 1) process-2-kia
                         (= modified-attack-roll 2) process-1-kia
                         (= modified-attack-roll 3) process-k-slash-3
                         (= modified-attack-roll 4) process-3-mc
                         (= modified-attack-roll 5) process-2-mc
                         (= modified-attack-roll 6) process-2-mc
                         (= modified-attack-roll 7) process-1-mc
                         (= modified-attack-roll 8) process-1-mc
                         (= modified-attack-roll 9) process-nmc
                         (= modified-attack-roll 10) process-ptc
                         (>= modified-attack-roll 11) process-no-result)]
    (result-fnc defender-location)))

(defn- process-8-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-3-kia
                         (= modified-attack-roll 1) process-2-kia
                         (= modified-attack-roll 2) process-1-kia
                         (= modified-attack-roll 3) process-k-slash-2
                         (= modified-attack-roll 4) process-2-mc
                         (= modified-attack-roll 5) process-2-mc
                         (= modified-attack-roll 6) process-1-mc
                         (= modified-attack-roll 7) process-1-mc
                         (= modified-attack-roll 8) process-nmc
                         (= modified-attack-roll 9) process-ptc
                         (>= modified-attack-roll 10) process-no-result)]
    (result-fnc defender-location)))

(defn- process-6-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-3-kia
                         (= modified-attack-roll 1) process-2-kia
                         (= modified-attack-roll 2) process-1-kia
                         (= modified-attack-roll 3) process-k-slash-2
                         (= modified-attack-roll 4) process-2-mc
                         (= modified-attack-roll 5) process-1-mc
                         (= modified-attack-roll 6) process-1-mc
                         (= modified-attack-roll 7) process-nmc
                         (= modified-attack-roll 8) process-ptc
                         (>= modified-attack-roll 9) process-no-result)]
    (result-fnc defender-location)))

(defn- process-4-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-2-kia
                         (= modified-attack-roll 1) process-1-kia
                         (= modified-attack-roll 2) process-k-slash-2
                         (= modified-attack-roll 3) process-2-mc
                         (= modified-attack-roll 4) process-1-mc
                         (= modified-attack-roll 5) process-1-mc
                         (= modified-attack-roll 6) process-nmc
                         (= modified-attack-roll 7) process-ptc
                         (>= modified-attack-roll 8) process-no-result)]
    (result-fnc defender-location)))

(defn- process-2-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-2-kia
                         (= modified-attack-roll 1) process-1-kia
                         (= modified-attack-roll 2) process-k-slash-1
                         (= modified-attack-roll 3) process-1-mc
                         (= modified-attack-roll 4) process-1-mc
                         (= modified-attack-roll 5) process-nmc
                         (= modified-attack-roll 6) process-ptc
                         (>= modified-attack-roll 7) process-no-result)]
    (result-fnc defender-location)))

(defn- process-1-attack [modified-attack-roll defender-location]
  (let [result-fnc (cond (<= modified-attack-roll 0) process-1-kia
                         (= modified-attack-roll 1) process-k-slash-1
                         (= modified-attack-roll 2) process-1-mc
                         (= modified-attack-roll 3) process-1-mc
                         (= modified-attack-roll 4) process-nmc
                         (= modified-attack-roll 5) process-ptc
                         (>= modified-attack-roll 6) process-no-result)]
    (result-fnc defender-location)))

(defn- process-0-attack [_ defender-location]
  defender-location)

(defn- analyze-fire-group-attack [{:keys [fire-group total-firepower total-drm]} cd wd defender-location]
  (let [attack-roll (+ cd wd)
        adjusted-fire-power (if (has-fire-group-cowered? fire-group cd wd)
                              (if (can-fire-group-double-cower? fire-group)
                                (adjust-fire-power-two-columns-to-the-left total-firepower)
                                (adjust-fire-power-one-column-to-the-left total-firepower))
                              total-firepower)
        modified-attack-roll (- attack-roll total-drm)
        process-attack-fnc (cond (>= adjusted-fire-power 36) process-36-attack
                                 (>= adjusted-fire-power 30) process-30-attack
                                 (>= adjusted-fire-power 24) process-24-attack
                                 (>= adjusted-fire-power 20) process-20-attack
                                 (>= adjusted-fire-power 16) process-16-attack
                                 (>= adjusted-fire-power 12) process-12-attack
                                 (>= adjusted-fire-power 8) process-8-attack
                                 (>= adjusted-fire-power 6) process-6-attack
                                 (>= adjusted-fire-power 4) process-4-attack
                                 (>= adjusted-fire-power 2) process-2-attack
                                 (>= adjusted-fire-power 1) process-1-attack
                                 :else process-0-attack)]
    (process-attack-fnc modified-attack-roll defender-location)))

(defn- execute-fire-group-attack
  ([] (list))
  ([defender-location fire-group]
   (let [fire-group-attack {:fire-group fire-group
                            :total-firepower (reduce calculate-firepower-of-attacker-location-in-fire-group 0 fire-group)
                            :total-drm (+ (total-drm-for-attacker fire-group) (total-drm-for-defender defender-location))}]
     (for [cd d6 wd d6]
       (analyze-fire-group-attack fire-group-attack cd wd defender-location)))))

(defn- execute-attacks [attacks defender-location]
  (reduce execute-fire-group-attack defender-location attacks))

; It is possible that two (or more) smcs can possess one weapon, so :units needs to be a list, and not just a single entity

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [game {:type :aslsk}
        german-elr {:elr 4}
        russian-elr {:elr 3}
        russian-infantry-initialize (partial infantry/initialize russian-elr)
        german-infantry-initialize (partial infantry/initialize german-elr)
        defender-location {:stack   (list {:possessions (list)
                                           :units       (list (russian-infantry-initialize r/nine-minus-one-leader))}
                                          {:possessions (list (sw/initialize r/mmg))
                                           :units       (list (russian-infantry-initialize r/elite-box-squad))})
                           :terrain :stone-building}
        attacker-location-1 {:stack (list {:possessions (list (sw/initialize r/dc))
                                           :units       (list (german-infantry-initialize g/nine-minus-one-leader))}
                                          {:possessions (list (sw/initialize g/lmg))
                                           :units       (list (german-infantry-initialize g/elite-circle-squad))})
                             :range 3}
        attacker-location-2 {:stack (list {:possessions (list)
                                           :units       (list (german-infantry-initialize g/first-line-squad))}
                                          {:possessions (list)
                                           :units       (list (german-infantry-initialize g/first-line-squad))})
                             :range 4}
        fire-group-1 (list attacker-location-1)
        fire-group-2 (list attacker-location-2)
        fire-group-3 (list attacker-location-1 attacker-location-2)
        attack-list-1 (list fire-group-1 fire-group-2)
        attack-list-2 (list fire-group-3)
        first-attack (execute-attacks attack-list-1 defender-location)
        second-attack (execute-attacks attack-list-2 defender-location)]
    (prn second-attack))
  (let [broken-four-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                 :when (does-unit-break? cd1 wd1 cd2 wd2 4 2 base-morale false)]
                             true)
        broken-six-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                :when (does-unit-break? cd1 wd1 cd2 wd2 6 2 base-morale false)]
                            true)
        broken-six-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                :when (does-unit-break? cd1 wd1 cd2 wd2 6 3 base-morale false)]
                            true)
        broken-four-up-two-then-six-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                    :when (or (does-unit-break? cd1 wd1 cd2 wd2 4 2 base-morale false)
                                                              (does-unit-break? cd3 wd3 cd4 wd4 6 3 base-morale false))]
                                                true)
        broken-one-eight-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                      :when (does-unit-break? cd1 wd1 cd2 wd2 8 2 base-morale false)]
                                  true)
        broken-one-eight-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                        :when (does-unit-break? cd1 wd1 cd2 wd2 8 3 base-morale false)]
                                    true)
        broken-one-twelve-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                         :when (does-unit-break? cd1 wd1 cd2 wd2 12 3 base-morale false)]
                                     true)
        broken-one-sixteen-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                          :when (does-unit-break? cd1 wd1 cd2 wd2 16 3 base-morale false)]
                                      true)
        broken-one-twenty-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6
                                         :when (does-unit-break? cd1 wd1 cd2 wd2 20 3 base-morale false)]
                                     true)
        broken-six-up-three-then-six-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                      :when (or (does-unit-break? cd1 wd1 cd2 wd2 6 3 base-morale false)
                                                                (does-unit-break? cd3 wd3 cd4 wd4 6 3 base-morale false))]
                                                  true)
        broken-eight-up-two-then-eight-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                      :when (or (does-unit-break? cd1 wd1 cd2 wd2 8 2 base-morale false)
                                                                (does-unit-break? cd3 wd3 cd4 wd4 8 3 base-morale false))]
                                                  true)
        broken-eight-up-two-then-twelve-up-three (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                       :when (or (does-unit-break? cd1 wd1 cd2 wd2 8 2 base-morale false)
                                                                 (does-unit-break? cd3 wd3 cd4 wd4 12 3 base-morale false))]
                                                   true)
        broken-eight-up-three-then-four-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                     :when (or (does-unit-break? cd1 wd1 cd2 wd2 8 3 base-morale false)
                                                               (does-unit-break? cd3 wd3 cd4 wd4 4 2 base-morale false))]
                                                 true)
        broken-four-up-one-then-eight-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                   :when (or (does-unit-break? cd1 wd1 cd2 wd2 8 2 base-morale false)
                                                             (does-unit-break? cd3 wd3 cd4 wd4 4 1 base-morale false))]
                                               true)
        broken-four-up-three-then-two-up-two (for [cd1 d6 wd1 d6 cd2 d6 wd2 d6 cd3 d6 wd3 d6 cd4 d6 wd4 d6
                                                   :when (or (does-unit-break? cd1 wd1 cd2 wd2 4 3 base-morale false)
                                                             (does-unit-break? cd3 wd3 cd4 wd4 2 2 base-morale false))]
                                               true)]
    ;    (println (float (/ (count broken-four-up-two) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-six-up-two) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-six-up-three) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-four-up-two-then-six-up-three) (* 6 6 6 6 6 6 6 6))))
    ;    (println (float (/ (count broken-one-eight-up-two) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-one-eight-up-three) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-one-twelve-up-three) (* 6 6 6 6))))
        (println (float (/ (count broken-one-sixteen-up-three) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-one-twenty-up-three) (* 6 6 6 6))))
    ;    (println (float (/ (count broken-six-up-three-then-six-up-three) (* 6 6 6 6 6 6 6 6))))
        (println (float (/ (count broken-eight-up-two-then-eight-up-three) (* 6 6 6 6 6 6 6 6))))
    ;    (println (float (/ (count broken-eight-up-two-then-twelve-up-three) (* 6 6 6 6 6 6 6 6))))
    ;    (println (float (/ (count broken-eight-up-three-then-four-up-two) (* 6 6 6 6 6 6 6 6))))
    (println (float (/ (count broken-four-up-three-then-two-up-two) (* 6 6 6 6 6 6 6 6))))
    (println (float (/ (count broken-four-up-one-then-eight-up-two) (* 6 6 6 6 6 6 6 6))))))

