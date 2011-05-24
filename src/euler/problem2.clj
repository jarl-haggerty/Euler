(let [fib<4mil (loop [accum [0 1]]
		 (if (>= (last accum) 4000000)
		   (pop accum)
		   (recur (conj accum (apply + (take-last 2 accum))))))]
  (reduce + (filter even? fib<4mil)))