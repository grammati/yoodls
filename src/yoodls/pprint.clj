(ns yoodls.pprint
  (:require (clojure [pprint :as pprint])))

(defn pprint-with-meta [obj]
  (let [orig-dispatch pprint/*print-pprint-dispatch*]
    (pprint/with-pprint-dispatch 
      (fn [o]
        (when (meta o)
          (print "^")
          (pprint/pprint (meta o))
          (pprint/pprint-newline :fill))
        (orig-dispatch o))
      (pprint/pprint obj))))

(defn pprint [obj]
  (if *print-meta*
    (pprint-with-meta obj)
    (pprint/pprint obj)))
