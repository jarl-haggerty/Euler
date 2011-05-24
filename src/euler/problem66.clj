(time (let [continue (fn [[a numerator denominator]]
		       (let [new-numerator (/ (- (first denominator) (* (second denominator) (second denominator)))
					      numerator)
			     integral (int (/ (+ (Math/sqrt (first denominator)) (second denominator))
					      new-numerator))
			     new-denominator [(first denominator) (- (- (second denominator) (* new-numerator integral)))]]
			 [integral new-numerator new-denominator]))
	    continued-fraction-coefficients (fn [n] (map first (iterate continue [(int (Math/sqrt n)) 1 [n (int (Math/sqrt n))]])))
	    sqrt (fn [n] (let [coefficients (continued-fraction-coefficients n)]
			   (map first (reductions (fn [[two one] a]
						    [one(/ (+ (* a (numerator one)) (numerator two))
							   (+ (* a (denominator one)) (denominator two)))
						     ])
						  [(Ratio. (BigInteger. (str (first coefficients)))
							   BigInteger/ONE)
						   (Ratio. (BigInteger. (str (inc (* (first coefficients) (second coefficients)))))
							   (BigInteger. (str (second coefficients))))]
						  (nthnext coefficients 2)))))
	    int-sqrt (fn [n] (if (zero? n)
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
	    square? (comp #(if (integer? %) % nil) int-sqrt)]
	(first (apply max-key #(numerator (second %))
		      (for [D (filter (complement square?) (range 1000000))]
			[D (first (drop-while #(not= (- (* (numerator %) (numerator %))
							(* D (denominator %) (denominator %))) 1)
					      (sqrt D)))])))))