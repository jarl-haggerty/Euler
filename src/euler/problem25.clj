(loop [accum [0 1] counter 1]
  (if (= (-> accum last str count) 100000)
    counter
    (recur [(last accum) (apply + accum)] (inc counter))))

(let [log (fn [base input]
	    (loop [current input result 0]
	      (if (>= (/ current base) 1)
		(recur (/ current base) (inc result))
		result)))
      move-fib (fn [accum direction]
		 (if (< direction 0)
		   [(- (second accum) (first accum)) (first accum)]
		   [(last accum) (apply + accum)]))
      digits 1000000
      cool-off 10]  
  (loop [accum [0 1] counter 1 direction 1 step 1000000 counter-accum 1]
    (cond
     (zero? (mod counter-accum step))
     (let [current-log (log 10 (last accum))]
       (println counter step)
       (cond
	(and (> current-log (dec digits))
	     (< 0 step))
	(recur (move-fib accum direction)
	       (+ counter direction)
	       (- direction)
	       (min -1 (int (/ step (- cool-off))))
	       (inc counter-accum))
	(and (< current-log (dec digits))
	     (> 0 step))
	(recur (move-fib accum direction)
	       (+ counter direction)
	       (- direction)
	       (max 1 (int (/ step (- cool-off))))
	       (inc counter-accum))
	(not= current-log (dec digits))
	(recur (move-fib accum direction)
	       (+ counter direction)
	       direction
	       step
	       (inc counter-accum))
	:else
	(loop [accum accum counter counter]
	  (if (< (log 10 (first accum)) (dec digits))
	    counter
	    (recur (move-fib accum -1) (dec counter))))))
     :else
     (recur (move-fib accum direction) (+ counter direction) direction step (inc counter-accum)))))