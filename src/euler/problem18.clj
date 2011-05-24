(use 'clojure.java.io)
(require '[clojure.string :as string])

(let [triangle (map (fn [x] (map #(Long/parseLong %) (.split x " ")))
		    (line-seq (reader "problem18.txt")))
      pairs (fn [input] (loop [stack input accum []]
			  (if (first stack)
			    (recur (rest stack) (conj accum (take 2 stack)))
			    accum)))]
  (loop [stack (-> triangle reverse rest) base (pairs (last triangle))]
    (if (first stack)
      (recur (rest stack) (pairs (map #(+ %1 (apply max %2)) (first stack) base)))	  
      (-> base first first))))