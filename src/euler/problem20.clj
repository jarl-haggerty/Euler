(let [factorial (fn [n] (apply * (range 1 (inc n))))
      log (fn [base input]
	    (loop [current input result 0]
	      (if (>= (/ current base) 1)
		(recur (/ current base) (inc result))
		result)))
      pow (fn [x y] (reduce * 1 (repeat y x)))
      digits (fn [input]
	       (for [x (range (log 10 input) -1 -1)]
		 (mod (bigint (/ input (pow 10 x))) 10)))]
  (->> 100 factorial digits (apply +)))