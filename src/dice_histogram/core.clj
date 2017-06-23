(ns dice-histogram.core
  (:gen-class))

(defn all-die-rolls
  "Return all possible die rolls from given dice. `dice` is a collection of
  integers. Each integer represents the number of sides for a single die.  For
  example, two six-sided dice would be [6 6]."
  [dice]
  (letfn [(helper [dice rolls-so-far]
            (if (empty? dice)
              [rolls-so-far]
              (let [[die & dice] dice]
                (->> die
                     inc
                     (range 1)
                     (reduce (fn [acc roll]
                               (concat acc (helper dice
                                                   (conj rolls-so-far roll))))
                             [])
                     vec))))]
    (helper dice [])))

#_ (all-die-rolls [6 6])
;; => [[1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [2 1] [2 2] [2 3] [2 4] [2 5] [2 6]
;; [3 1] [3 2] [3 3] [3 4] [3 5] [3 6] [4 1] [4 2] [4 3] [4 4] [4 5] [4 6] [5 1]
;; [5 2] [5 3] [5 4] [5 5] [5 6] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6]]


(defn dice-histogram
  "Create histogram data for all posible values that can be made by rolling
  given dice.  Returns a vector of [v k] where v is the value and k is the
  number of rolls that sum to that value."
  [dice]
  (->> dice
       all-die-rolls
       (reduce (fn [acc rolls]
                 (let [sum (reduce + 0 rolls)]
                   (assoc acc sum (inc (or (acc sum)
                                           0)))))
               {})
       (into (sorted-map))
       (into [])))


#_ (dice-histogram [6 6])
;; => [[2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 5] [9 4] [10 3] [11 2] [12 1]]


(defn simulate-rolls
  "Simulate a number of dice rolls with the given dice and return a histogram."
  [dice num-of-rolls]
  (->> (range num-of-rolls)
       (reduce (fn [acc roll-number]
                 (let [sum (reduce (fn [acc die]
                                      (+ acc (rand-int die)))
                                    0
                                    dice)]
                   (assoc acc sum
                          (inc (or (acc sum)
                                   0)))))
               {})
       (into (sorted-map))
       (into [])))

#_ (simulate-rolls [6 6] 10)
;; => [[2 1] [3 2] [4 1] [5 2] [6 2] [7 1] [8 1]]

(defn print-histogram
  "Prints a histogram graphically."
  [h]
  (doseq [[v k] h]
    (print (format "%4d: " v))
    (dotimes [n k]
      (print "#"))
    (println)))

;; dice-histogram.core> (print-histogram (dice-histogram [6 6 6]))
;;    3: #
;;    4: ###
;;    5: ######
;;    6: ##########
;;    7: ###############
;;    8: #####################
;;    9: #########################
;;   10: ###########################
;;   11: ###########################
;;   12: #########################
;;   13: #####################
;;   14: ###############
;;   15: ##########
;;   16: ######
;;   17: ###
;;   18: #

(defn -main
  ""
  [& args]
  (print-histogram (dice-histogram [6 6 6])))
