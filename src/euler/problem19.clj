(ns euler.problem19)

(let [months {0 31
	      1 28
	      2 31
	      3 30
	      4 31
	      5 30
	      6 31
	      7 31
	      8 30
	      9 31
	      10 30
	      11 31}]
  (loop [day 0 month 0 year 0 mondays 0]
  (let [new-day (mod (+ day (get months month) (if (and (= month 1) (not= year 0) (zero? (mod year 4))) 1 0)) 7)]
    (if (= year 101)
      mondays
      (if (> year 0)
        (recur new-day (mod (inc month) 12) (if (= month 11) (inc year) year) (if (= day 1) (inc mondays) mondays))
        (recur new-day (mod (inc month) 12) (if (= month 11) (inc year) year) mondays))))))