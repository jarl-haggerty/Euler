(let [truncate-left (fn [input] (loop [stack input accum []]
				 (if (first stack)
				   (recur (rest stack) (conj accum stack))
				   accum)))
      truncate-right (fn [input] (loop [stack (reverse input) accum []]
				 (if (first stack)
				   (recur (rest stack) (conj accum (reverse stack)))
				   accum)))
      prime? (fn [input]
	       (cond
		(<= input 1) false
		(= input 2) true
		:else (every? not
			      (for [n (range 2 (inc (Math/sqrt input)))]
				(zero? (mod input n))))))]
  
  (reduce + (take 11 (filter
		      (fn [number]
			(let [number-str (-> number str seq)
			      left (->> number-str truncate-left (map #(apply str %)) (map #(Long/parseLong %)))
			      right (->> number-str truncate-right (map #(apply str %)) (map #(Long/parseLong %)))]
					;		(println (conj (concat left right) number))
			  (every? identity (map prime?  (concat left right)))))
		      (range 8 Double/POSITIVE_INFINITY)))))