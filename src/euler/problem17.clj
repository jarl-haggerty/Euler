(let [digit (fn [input]
	      (cond
	       (= input 1) "one"
	       (= input 2) "two"
	       (= input 3) "three"
	       (= input 4) "four"
	       (= input 5) "five"
	       (= input 6) "six"
	       (= input 7) "seven"
	       (= input 8) "eight"
	       (= input 9) "nine"
	       :else ""))
      word (fn [input]
	     (cond 
	      (= input 1000) 
	      "onethousand"
	      (= input 0)
	      "zero"
	      true
	      (str
	       (if (> input 99)
		 (str (digit (int (/ input 100))) "hundred")
		 "")
	       (if (and (> input 99) (> (mod input 100) 0)) "and" "")
	       (let [current (mod input 100) tens (int (/ current 10)) ones (mod current 10)]
		 (cond
		  (= current 10) "ten"
		  (= current 11) "eleven"
		  (= current 12) "twelve"
		  (= current 13) "thirteen"
		  (= current 14) "fourteen"
		  (= current 15) "fifteen"
		  (= current 16) "sixteen"
		  (= current 17) "seventeen"
		  (= current 18) "eighteen"
		  (= current 19) "nineteen"
		  true
		  (str 
		   (cond
		    (= tens 2) "twenty"
		    (= tens 3) "thirty"
		    (= tens 4) "forty"
		    (= tens 5) "fifty"
		    (= tens 6) "sixty"
		    (= tens 7) "seventy"
		    (= tens 8) "eighty"
		    (= tens 9) "ninety")
		   (digit ones)))))))]
  (count (filter #(not= % \space) (apply str (for [x (range 1 1001)] (do (println (word x)) (word x)))))))