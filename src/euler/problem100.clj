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
						  (nthnext coefficients 2)))))]
	(inc (numerator (first (drop-while #(or (<= (denominator %) 1000000000000)
						(not= (- (* (numerator %) (numerator %))
							 (* 2 (denominator %) (denominator %))) 1))
					   (sqrt 2)))))))