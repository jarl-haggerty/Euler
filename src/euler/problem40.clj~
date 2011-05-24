(ns euler.problem40
  (:use euler.core))

(defn digit [input]
  (loop [number 1 accum 1]
    (if (< accum input)
      (recur (inc number) (+ accum (digits number)))
      (last (take (inc (- accum input)) (int-to-list number))))))

(time (println (reduce * (map #(digit (pow 10 %)) (range 0 7)))))
