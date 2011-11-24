(ns yoolds.lazy-map)


(defprotocol LazyMapValue
  (force-value [this] "Applied to each value extracted from a lazy map."))

(extend-protocol LazyMapValue
  Object
  (force-value [this] this)
  clojure.lang.Delay
  (force-value [this] (deref this))
  java.util.concurrent.Future
  (force-value [this] (deref this))
  )


(defn lazy-map*
  "Wraps the given map such that any value that is a Delay will be
  forced when extracted."
  [& [m]]
  (proxy [clojure.lang.APersistentMap
          clojure.lang.IObj
          clojure.lang.IEditableCollection] []
    
    ;; This method implements the "real" funtionality:
    (valAt
      ([k] (force-value (get m k)))
      ([k not-found] (force-value (get m k not-found))))

    ;; These methods are to preserve the type by returning
    ;; new lazy-map* instances when required.
    (assoc [k v]
      (lazy-map* (assoc m k v)))
    ;;(assocEx [k v] ???)
    (without [k]
      (lazy-map* (dissoc m k)))
    (withMeta [md]
      (lazy-map* (with-meta m md)))

    ;; boilerplate - just delegate:
    (containsKey [k] (contains? m k))
    (count [] (count m))
    (entryAt [k] (.entryAt ^clojure.lang.Associative m k))
    (iterator [] (.iterator ^java.util.Map m))
    (meta [] (meta m))
    (seq [] (seq m))
    ;(seq [] (for [[k v] m] (clojure.lang.MapEntry. k (force-value v))))

    (asTransient []
      (throw (Exception. "I don't know how to implement this yet.")))
    ))


(defn- lazify
  [form]
  (if (and (list? form)
           (symbol? (first form))
           (not (#{'future 'delay} (first form))))
    (list `delay form)
    form))

(defmacro lazy-map
  "Creates a lazy-map from an alternating list of keys and values.
   A lazy map is equivalent to a normal clojure map except in 2 cases:
   1) When a value is a delay, it is evaluated only when first accessed.
   2) When a value is a future, it is dereferenced when accessed.

   This macro helps with the creation the lazy map by wrapping any value
   that appears to be a function call in a delay, unless it appears to
   be a future.

   Example:
   (lazy-map :v \"normal value\"
             :d (do (println \"delay evaluated\") :delay)
             :f (future (Thread/sleep 2000) :future))

   :d's value will be evaluated only if accessed.
   :f's value will be calculated in a thread, and accessing it could block.
  "
  [& kvs]
  (when (odd? (count kvs))
    (throw (IllegalArgumentException. "lazy-map requires an even number of forms")))
  (list `lazy-map*
        (reduce (fn [m [k v]]
                  (.assocEx m k (lazify v)))
                {}
                (partition 2 kvs))))

(defmacro lazy-assoc
  ""
  [m & keyvals]
  `(lazy-map* (merge ~m (lazy-map ~@keyvals))))


(comment
  (def test (lazy-map
             :foo (do (println "evaluated!") :bar)
             :xxx "yyy")))
