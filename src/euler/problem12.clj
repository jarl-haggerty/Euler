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
  (first (for [x (map #(reduce + (range  %)) (range 1 Double/POSITIVE_INFINITY))
	       :when (> (apply * (map inc (vals (prime-factorization x)))) 500)]
	   x)))
