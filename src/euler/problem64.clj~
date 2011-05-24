(time (letfn [(sqrt [x] (let [root (Math/sqrt x)
			      dec-root (int root)
			      inc-root (inc dec-root)]
			  (condp = x
			      (* dec-root dec-root) dec-root
			      (* inc-root inc-root) inc-root
			      root)))
	      (continue [[numerator denominator]]
			(let [new-numerator (/ (- (first denominator) (* (second denominator) (second denominator)))
					       numerator)
			      integral (int (/ (+ (Math/sqrt (first denominator)) (second denominator))
					       new-numerator))
			      new-denominator [(first denominator) (- (- (second denominator) (* new-numerator integral)))]]
			  [new-numerator new-denominator]))]
	(count (for [x (filter #(not (integer? (sqrt %))) (range 1 10000))
		     :let [x0 [1 [x (int (Math/sqrt x))]]
			   tortise (iterate continue x0) hare (iterate (comp continue continue) x0)
			   series (take-while #(not= (first %) (second %)) (map vector tortise hare))
			   cycle-point (continue (last (cons x0 (map first series))))
			   cycle (inc (count (take-while #(not= % cycle-point)
							 (rest (iterate continue cycle-point)))))]
		     :when (odd? cycle)]
		 cycle))))