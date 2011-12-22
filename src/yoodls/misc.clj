(ns yoodls.misc)


;;; dissoc-in from core.incubator behaves badly with an empty list of keys
(defn dissoc-in [m ks]
  (let [k (last ks)
        ks (butlast ks)]
    (if ks
      (if-let [mm (get-in m ks)]
        (assoc-in m ks (dissoc mm k))
        m)
      (if k
        (dissoc m k)
        m))))



(defmacro mfor
  "Map comprehension.

   Builds a map from a for expression.

   Example:

     (mfor [item col]
       (make-key item) (make-value item))

   is equivalent to, but more efficient than:

     (into {}
       (for [item col]
         [(make-key item) (make-value item)]))

   mfor avoids creating both the intermediate seq and the intermediate
   two-element vectors.

   Note: the body must contain exactly two expressions.
   "
  [[& iters] key-expr val-expr]
  `(let [m# (atom (transient {}))]
     (doseq [~@iters]
       (swap! m# assoc! ~key-expr ~val-expr))
     (persistent! @m#)))


