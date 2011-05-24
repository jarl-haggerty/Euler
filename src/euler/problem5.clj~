(let [prime? (fn [input]
	       (cond
		(<= input 1) false
		(= input 2) true
		:else (every? not
			      (for [n (range 2 (inc (Math/sqrt input)))]
				(zero? (mod input n))))))
      pow (fn [x y] (reduce * 1 (repeat y x)))
      primes (filter prime? (range (Math/sqrt 600851475143)))
      prime-factorization (fn [input]
			    (cond
			     (< input 2) {}
			     :else (loop [stack primes current input accum {}]
				     (if (zero? (mod current (first stack)))
				       (if (= current (first stack))
					 (merge-with + accum {(first stack) 1})
					 (recur stack (/ current (first stack)) (merge-with + accum {(first stack) 1})))
				       (recur (rest stack) current accum)))))]
  (reduce #(* %1 (pow (first %2) (second %2))) 1 (apply merge-with max (map prime-factorization (range 1 21)))))