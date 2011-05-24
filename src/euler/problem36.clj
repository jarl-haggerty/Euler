(let [palindrome? (fn [input] (= (seq input) (reverse input)))]
  (reduce + (filter #(and (-> % Integer/toString palindrome?)
			  (-> % Integer/toBinaryString palindrome?))
		    (range 1000000))))