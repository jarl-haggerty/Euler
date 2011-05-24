(ns euler.problem32
  (:use euler.core))

(defn sol2
  (loop [x (range 1000000000) accum []]
    (if (empty? x)
      counter
      (recur (rest x)
             (loop [y (range (inc x)) inner-accum []]
               (if (empty? y)
                 inner-accum
                 (let [x-log (log 10 (first x))
                       y-log (log 10 (first y))]
                   (if (and
                        (= 9 (+ x-log y-log (* x-log y-log) 3))
                        (not (contains? accum (* x y)))
                        (= #{1 2 3 4 5 6 7 8 9}
                           (set (concat (int-to-list (first x))
                                        (int-to-list (first y))
                                        (int-to-list (* (first x) (first y)))))))
                     (recur (rest y) (conj inner-accum (* (first x) (first y))))
                     (recur (rest y) (inner-accum))))))))))
