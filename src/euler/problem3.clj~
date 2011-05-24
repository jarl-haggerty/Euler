(let [prime? (fn [input]
	       (cond
		(<= input 1) false
		(= input 2) true
		:else (every? not
			      (for [n (range 2 (inc (Math/sqrt input)))]
				(zero? (mod input n))))))
      primes (filter prime? (range (Math/sqrt 600851475143)))]
  (reduce max (filter #(= (mod 600851475143 %) 0) primes)))