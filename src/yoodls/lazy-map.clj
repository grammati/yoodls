(ns yoolds.lazy-map)


(defn delay-map
  "Wraps the given map such that any value that is a Delay will be
  forced when extracted."
  [& [m]]
  (proxy [clojure.lang.APersistentMap
          clojure.lang.IObj
          clojure.lang.IEditableCollection] []
    
    ;; This method implements the "real" funtionality:
    (valAt
      ([k] (force (get m k)))
      ([k not-found] (force (get m k not-found))))

    ;; These methods are to preserve the type by returning
    ;; new delay-map instances when required.
    (assoc [k v]
      (delay-map (assoc m k v)))
    ;;(assocEx [k v] ???)
    (without [k]
      (delay-map (dissoc m k)))
    (withMeta [md]
      (delay-map (with-meta m md)))

    ;; boilerplate - just delegate:
    (containsKey [k] (contains? m k))
    (count [] (count m))
    (entryAt [k] (.entryAt ^clojure.lang.Associative m))
    (iterator [] (.iterator ^java.util.Map m))
    (meta [] (meta m))
    (seq [] (seq m))

    (asTransient []
      (throw (Exception. "I don't know how to implement this yet.")))
    ))



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

(defmacro lazy-assoc [m & keyvals]
  `(delay-map (merge ~m (lazy-map ~@keyvals))))


(comment
  (def test (lazy-map
             :foo (do (println "evaluated!") :bar)
             :xxx "yyy")))
