(use 'clojure.java.io)

(let [log (fn [base input]
	    (loop [current input result 0]
	      (if (>= (/ current base) 1)
		(recur (/ current base) (inc result))
		result)))
      pow (fn [x y] (reduce * 1 (repeat y x)))
      digits (fn [input]
	       (for [x (range (log 10 input) -1 -1)]
		 (mod (bigint (/ input (pow 10 x))) 10)))
      list-to-int (fn [input]
		    (loop [stack input power (dec (count input)) accum 0]
		      (if (> power -1)
			(recur (rest stack) (dec power) (+ accum (* (first stack) (pow 10 power))))
			accum)))
      numbers (map #(BigInteger. %) (line-seq (reader "problem13.txt")))]
  (list-to-int (take 10 (digits (reduce #(-> (+ %1 %2)) numbers)))))

;(def tens (log 10 (reduce + a)))
;(println (bigint (/ (reduce + a) (pow 10 (- tens 9)))))
