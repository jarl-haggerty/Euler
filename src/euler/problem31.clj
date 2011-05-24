(let [currencies #{1 2 5 10 20 50 100 200}
      goal 200]
  (loop [open (set (map vector currencies)) solutions #{}]
    (if (empty? open)
      (count solutions)
      (let [new-solutions (filter #(= (reduce + %) goal) open)
	    new-open (set
		      (filter #(<= (reduce + %) goal)
			      (apply concat
				     (map (fn [x]
					    (map (fn [y] (conj x y))
						 (filter #(<= % (last x)) currencies)))
					  (difference open new-solutions)))))]
	(recur new-open (union solutions new-solutions))))))
