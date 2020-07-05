(ns consistent-hash.core
  (:require [clojure.math.numeric-tower :as math]
            )
  )

(def m (math/expt 2 32))

(defn consistent-hash [x number-of-nodes]
  (math/ceil (/ (mod x m) (int (/ m number-of-nodes)))))

(defn average [num-seq]
  (/ (apply + num-seq) (count num-seq)))

(defn standard-deviation [num-seq]
  (let [avg (average num-seq)]
    (->> (map (fn [x]
                (let [n (math/abs (- x avg))]
                  (* n n)
                  )
                ) num-seq)
         average
         math/sqrt
         )
    )
  )

(defn -main [& args]
  (let [number-of-nodes 10
        times 1000000
        hash-values  (repeatedly times (fn [] (consistent-hash (long (rand m)) number-of-nodes)))
        loads (sort (reduce (fn [ret node] (update ret node inc)) (zipmap (range 1 (inc number-of-nodes)) (repeat 0)) hash-values))
        ]
    (println "Server load: " loads)
    (println "Standard deviation: " (standard-deviation (map second loads)))
    )
  )

