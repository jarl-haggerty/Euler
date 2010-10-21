(ns euler.core
  (:use clojure.set))

(defn is-prime [input]
  (cond
    (<= input 1) false
    (= input 2) true
    :else (every? not
           (for [n (range 2 (inc (Math/sqrt input)))]
             (zero? (mod input n))))))

(def primes (filter is-prime (range Double/POSITIVE_INFINITY)))

(defn prime-range [start] (filter is-prime (range start Double/POSITIVE_INFINITY)))

(defn prime-factorization [input]
  (cond
    (< input 2) {}
    :else (loop [stack primes current input accum {}]
           (if (zero? (mod current (first stack)))
             (if (= current (first stack))
               (merge-with + accum {(first stack) 1})
               (recur stack (/ current (first stack)) (merge-with + accum {(first stack) 1})))
             (recur (rest stack) current accum)))))

(defn log [base input]
  (loop [current input result 0]
    (if (< (/ current base) 1)
      (recur (/ current base) (inc result))
      result)))

(defn digits [input] (inc (log 10 input)))

(defn pow [x y]
  (cond
   (= y 0) 1
   (= y 1) x
   :else (recur (* x x) (dec y))))

(defn int-to-list [input]
  (for [x (range (log 10 input) -1 -1)]
    (mod (bigint (/ input (pow 10 x))) 10)))

(defn inc-indices [input dimensions]
  (loop [l-input input l-dimensions dimensions result []]
    (if (empty? l-input)
      result
      (if (= (first l-input) (first l-dimensions))
        (recur (rest l-input) (rest l-dimensions) (conj result 0))
        (concat 
          (conj result (inc (first l-input))) 
          (rest l-input))))))

(defn divisors [input]
  (let [factorization (prime-factorization input)]
    (loop [powers (take (count factorization) (cycle [0])) accum []]
      (let [new-powers (inc-indices powers (vals factorization))]
        (if (every? zero? new-powers)
          accum
          (recur new-powers (conj accum (reduce * (map #(pow %1 %2) (keys factorization) powers)))))))))

(defn factorial 
  ([n bottom]
    (loop [current n accum 1]
      (if (<= current bottom)
        accum
        (recur (dec current) (* accum current)))))
  ([n] (factorial n 1)))

(defn choose [m n] 
  (/ (factorial m (- m n)) (factorial n)))

(defn binomial-distribution [trials successes odds]
  (* (choose trials successes) (pow odds successes) (pow (- 1 odds) (- trials successes))))

(defn negative-binomial-distribution [trials successes odds]
  (* (choose (dec trials) (dec successes)) (pow odds successes) (pow (- 1 odds) (- trials successes))))

(defn hypergeometric [N K n x]
  (/ (* (choose K x) (choose (- N K) (- n x))) (choose N n)))

(defn permutations [input]
  (loop [current (map list input)]
    (if (= (count (first current)) (count input))
      current
      (recur
        (for [c current i (reduce disj (set input) c)]
          (cons i c))))))

(defn combinations [input length]
  (loop [current (map list input)]
    (if (= (count (first current)) length)
      current
      (recur
        (for [c current i input]
          (cons i c))))))

(defn fib [input]
  (loop [index 0 accum [0 1]]
    (if (= index input)
      (first accum)
      (recur (inc index) [(second accum) (reduce + accum)]))))

(defn gcd [a b]
  (loop [x (max a b) y (min a b) z (mod x y)]
    (if (zero? z)
      y
      (recur y z (mod y z)))))

(defn lcm [a b]
  (loop [factorization (merge-with max (prime-factorization a) (prime-factorization b)) accum 1]
    (if (empty? factorization)
      accum
      (recur (rest factorization) (* accum (pow (first (first factorization)) (second (first factorization))))))))

(defn detect-cycle
  ([series]
     (loop [tortoise (rest series) hare (rest (rest series))]
       (if (= (first tortoise) (first hare))
         tortoise
         (recur (rest tortoise) (rest hare)))))
  ([function & x0]
     (loop [tortoise (apply function x0) hare (apply function (apply function x0))]
       (if (= tortoise hare)
         tortoise
         (recur (apply function tortoise) (apply function (apply function hare)))))))

(defn count-cycle
  ([where]
     (loop [hare (rest where) accum 1]
       (if (= (first hare) (first where))
         accum
         (recur (rest hare) (inc accum)))))
  ([function & where]
     (loop [hare (apply function where) accum 1]
       (if (= where hare)
         accum
         (recur (apply function hare) (inc accum))))))

(defn fractional-part [input] (- input (int input)))
(defn integral-part [input] (int input))

(defn mean [& args] (/ (reduce + args) (count args)))
(defn median [& args]
  (let [middle (int (/ (count args) 2))
        data (apply vector (sort args))]
    (if (even? (count args))
      (mean (data (dec middle)) (data middle))
      (data middle))))
(defn data-range [& args] (- (apply max args) (apply min args)))
(defn variance [& args]
  (let [middle (apply mean args)]
    (/ (reduce + (map #(pow (- middle %) 2) args)) (count args))))
(defn standard-deviation [& args] (Math/sqrt (apply variance args)))
(defn first-quartile [& args]
  (let [location (/ (+ (count args) 3) 4)
        data (apply vector (sort args))]
    (+ (data (int location))
       (* (- (data (inc (int location))) (data (int location))) (fractional-part location)))))
(defn third-quartile [& args]
  (let [location (/ (inc (* 3 (count args))) 4)
        data (apply vector (sort args))]
    (+ (data (int location))
       (* (- (data (inc (int location))) (data (int location))) (fractional-part location)))))
(defn inter-quartile [& args] (- (apply third-quartile args) (apply first-quartile args)))

(defn integrate [& args]
  (loop [stack (rest args) result [(first args)]]
    (if (empty? stack)
      result
      (recur
       (rest stack)
       (conj result (+ (last result) (first stack)))))))
(defn count-when [pred data]
  (reduce +
    (for [d data]
      (if (pred d) 1 0))))
(defn bin [num-bins & args]
  (let [bin-size (/ (apply data-range args) num-bins)
        bottom (apply min args)
        top (apply max args)]
      (for [b (range num-bins)]
        [(+ bottom (* b bin-size)) (+ bottom (* (inc b) bin-size))
         (count-when
          #(and
            (>= % (+ bottom (* b bin-size)))
            (< % (+ bottom (* (inc b) bin-size))))
          args)])))

(def currencies #{1 2 5 10 20 50 100 200})
(def goal 200)

(defn sol []
  (loop [open (set (map vector currencies)) solutions #{}]
    (println (count open) (count solutions))
    (if (empty? open)
      solutions
      (let [new-solutions (filter #(= (reduce + %) goal) open)
            new-open (set
                      (filter #(<= (reduce + %) goal)
                              (apply concat
                                     (map (fn [x]
                                            (map (fn [y] (conj x y))
                                                 (filter #(<= % (last x)) currencies)))
                                          (difference open new-solutions)))))]
        (recur new-open (union solutions new-solutions))))))


(defn sol2
  (loop [x (range 1000000000) accum []]
    (if (empty? x)
      counter
      (recur (rest x)
             (loop [y (range (inc x)) inner-accum []]
               (if (empty? y)
                 inner-accum
                 (let [x-log (log 10 (first x))
                       y-log (log 10 (first y))]
                   (if (and
                        (= 9 (+ x-log y-log (* x-log y-log) 3))
                        (not (contains? accum (* x y)))
                        (= #{1 2 3 4 5 6 7 8 9}
                           (set (concat (int-to-list (first x))
                                        (int-to-list (first y))
                                        (int-to-list (* (first x) (first y)))))))
                     (recur (rest y) (conj inner-accum (* (first x) (first y))))
                     (recur (rest y) (inner-accum))))))))))
