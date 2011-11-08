(ns yoolds.lazy-map)


(defn delay-map
  ([]
     (delay-map nil))
  ([m]
     (letfn [(value-of [v]
               (if (instance? clojure.lang.IDeref v) @v v))]
       (proxy [clojure.lang.APersistentMap] []
         (assoc [k v]
           (delay-map (assoc m k v)))
         (without [k]
           (delay-map (dissoc m k)))
         (valAt
           ([k]
              (.valAt this k nil))
           ([k not-found]
              (value-of (get m k not-found))))
         (seq []
           ;(for [[k v] (seq m)] [k (value-of v)]) ;; ???
           (seq m))
         (count []
           (count m))
         ))))



(defmacro lazy-map [& keyvals]
  (when (odd? (count keyvals))
    (throw (Exception. "lazy-map requires an even number of forms")))
  (list
   `delay-map
   (into {}
         (for [[k v] (partition 2 keyvals)]
           [k (if (and (list? v) (symbol? (first v)))
                (list `delay v)
                v)]))))

(def test (lazy-map
           :foo (do (println "evaluated!") :bar)
           :xxx "yyy"))
