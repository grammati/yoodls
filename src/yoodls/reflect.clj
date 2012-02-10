(ns yoodls.reflect
  (:require (clojure [reflect :as reflect]
                     [pprint :as pp])))

(defn print-public-methods [o]
  (->> o
       reflect/reflect
       :members
       (filter (comp :public :flags))
       (sort-by :name)
       (pp/print-table [:name :return-type :parameter-types])))

(def ppm print-public-methods)
