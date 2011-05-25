(import 'clojure.lang.Ratio)
(import 'java.math.BigInteger)
(time (let [squares (set (take-while #(< % 1000) (map #(* % %) (range))))
	    continue (fn [[a numerator denominator]]
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
							   (+ (* a (denominator one)) (denominator two)))])
						  [(Ratio. (BigInteger. (str (first coefficients)))
							   BigInteger/ONE)
						   (Ratio. (BigInteger. (str (inc (* (first coefficients) (second coefficients)))))
							   (BigInteger. (str (second coefficients))))]
						  (nthnext coefficients 2)))))]
	(first (reduce (fn [D1 D2]
			 (let [new-x (numerator (first (drop-while #(not= (- (* (numerator %) (numerator %))
									     (* D2 (denominator %) (denominator %)))
									  1)
								   (sqrt D2))))]
			   (if (> (second D1) new-x)
			     D1 [D2 new-x])))
		       [-1 -1]
		       (filter (complement squares) (range 1000))))))