(use 'clojure.set)

(let [prime? (fn [input]
	       (cond
		(<= input 1) false
		(= input 2) true
		:else (every? not
			      (for [n (range 2 (inc (Math/sqrt input)))]
				(zero? (mod input n))))))
      primes (filter prime? (range Double/POSITIVE_INFINITY))
      prime-factorization (fn [input]
			    (cond
			     (< input 2) {}
			     :else (loop [stack primes current input accum {}]
				     (if (zero? (mod current (first stack)))
				       (if (= current (first stack))
					 (merge-with + accum {(first stack) 1})
					 (recur stack (/ current (first stack)) (merge-with + accum {(first stack) 1})))
				       (recur (rest stack) current accum)))))
      pow (fn [x y] (reduce * 1 (repeat y x)))
      inc-indices (fn [input dimensions]
		    (loop [l-input input l-dimensions dimensions result []]
		      (if (empty? l-input)
			result
			(if (= (first l-input) (first l-dimensions))
			  (recur (rest l-input) (rest l-dimensions) (conj result 0))
			  (concat 
			   (conj result (inc (first l-input))) 
			   (rest l-input))))))
      divisors (fn [input]
		 (let [factorization (prime-factorization input)]
		   (loop [powers (take (count factorization) (cycle [0])) accum []]
		     (let [new-powers (inc-indices powers (vals factorization))]
		       (if (every? zero? new-powers)
			 accum
			 (recur new-powers (conj accum (reduce * (map #(pow %1 %2) (keys factorization) powers)))))))))
      abundant-numbers (filter #(< % (reduce + (divisors %))) (range 1 28123))]
  (reduce + (difference (set (range 1 28123))
			(set (for [x abundant-numbers y (filter #(<= % x) abundant-numbers)]
			       (+ x y))))))
