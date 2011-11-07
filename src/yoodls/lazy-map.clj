(ns yoolds.lazy-map)



#_(defn delay-map [m]
  (proxy [clojure.lang.APersistentMap] []
    (assoc [k v] (delay-map (assoc m k v)))
    (without [k] (delay-map (dissoc m k)))
    (valAt [k not-found]
      (let [v (get m k not-found)]
        (if (instance? clojure.lang.IDeref v) @v v)))
    (valAt [k]
      (.valAt this k nil))))


(defn delay-map [m]
  (reify
    clojure.lang.IPersistentMap
    (assoc [this k v] (delay-map (assoc m k v)))
    (without [this k] (delay-map (dissoc m k)))
    (valAt [this k not-found]
      (let [v (get m k not-found)]
        (if (instance? clojure.lang.IDeref v) @v v)))
    (valAt [this k]
      (.valAt this k nil))))

