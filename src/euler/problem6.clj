(let [sum (reduce + (range 101))
      square-of-sums (* sum sum)
      sum-of-squares (reduce + (map #(* % %) (range 101)))]
  (Math/abs (- square-of-sums sum-of-squares)))