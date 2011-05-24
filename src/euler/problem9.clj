(first (for [a (range 1 1000) b (range (inc a) 1000)
	     :let [c (- 1000 a b)]
	     :when (and (= (+ (* a a) (* b b)) (* c c)))]
	 (* a b c)))