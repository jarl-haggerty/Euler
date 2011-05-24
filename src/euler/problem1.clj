(use 'clojure.set)

(reduce + (union (set (range 0 1000 3)) (set (range 0 1000 5))))