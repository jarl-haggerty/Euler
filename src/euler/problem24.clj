(let [permutations (fn [input]
		     (let [sorted-input (apply sorted-set input)]
		       (loop [current (map vector input)]
			 (if (= (count (first current)) (count input))
			   current
			   (recur
			    (for [c current i (reduce disj sorted-input c)]
			      (conj c i)))))))]
  (nth (permutations (range 10)) 1000000))