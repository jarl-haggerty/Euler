(let [names (-> "problem22.txt" reader line-seq first (.replace "\"" "") (.replace " " "") (.split ",") seq sort)]
  (reduce + (map-indexed (fn [index name]
			   (* (inc index) (reduce + (map #(- (int %) 64) name))))
			 names)))
