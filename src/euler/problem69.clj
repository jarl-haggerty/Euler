(time (let [prime? (fn [input]
		     (cond
		      (<= input 1) false
		      (= input 2) true
		      :else (every? not
				    (for [n (range 2 (inc (Math/sqrt input)))]
				      (zero? (mod input n))))))
	    primes (filter prime? (range))]
	(first (last (take-while #(< (first %) 1000000)
				 (iterate #(cons (* (first %) (second %)) (nthnext % 2)) primes))))))

(time (let [sqrt (fn [n] (if (zero? n)
			   0
			   (let [initial (* 2 (Math/sqrt n))]
			     (loop [guess (- initial (/ (- (* initial initial) initial) (* 2 initial))) last-guess initial]
			       (if (< (- last-guess guess) 1/10)
				 (let [dec-guess (bigint guess)
				       inc-guess (inc dec-guess)]
				   (condp = n
				       (* dec-guess dec-guess) dec-guess
				       (* inc-guess inc-guess) inc-guess
				       guess))
				 (recur (- guess (/ (- (* guess guess) n) (* 2 guess)))
					guess))))))
	    square? (comp #(if (integer? %) % nil) sqrt)]
	(first (filter square? (map #(inc (* 2N % %)) (range 1000000000000 Double/POSITIVE_INFINITY))))))