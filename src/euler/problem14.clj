(let [collatz (fn [input lengths]
		(loop [current input accum 0]
		  (if (contains? lengths current)
		    (assoc lengths input (+ accum (get lengths current)))
		    (if (zero? (mod current 2))
		      (recur (/ current 2) (inc accum))
		      (recur (inc (* 3 current)) (inc accum))))))]
  (loop [stack (range 2 1000000) lengths {(long 1) (long 1)}]
    (if (first stack)
      (recur (rest stack) (collatz (long (first stack)) lengths))
      (first (apply max-key second lengths)))))