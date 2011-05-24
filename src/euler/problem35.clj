(let [circulations (fn [input]
		     (loop [ahead input behind [] accum []]
		       (if (first ahead)
			 (recur (rest ahead) (conj behind (first ahead)) (conj accum (concat ahead behind)))
			 accum)))
      prime? (fn [input]
	       (cond
		(<= input 1) false
		(= input 2) true
		:else (every? not
			      (for [n (range 2 (inc (Math/sqrt input)))]
				(zero? (mod input n))))))
      primes (set (filter prime? (map long (cons 2 (range 1 1000000 2)))))]
  (count (filter
	  (fn [prime]
	    (let [p (->> prime str circulations (map #(apply str %)) (map #(Long/parseLong %)))]
	      (every? identity (map #(contains? primes %) p))))
	  primes)))