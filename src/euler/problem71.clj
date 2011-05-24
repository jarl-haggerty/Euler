(time (let [lcm (fn [a b] (if (> a b)
			    (recur b a)
			    (first (drop-while #(not= (mod % a) 0) (iterate #(+ % b) b)))))
	    step (/ 1 (some #(if (zero? (mod % 7)) % nil) (range 999999 0 -7)))]
	(numerator (- 3/7 step))))