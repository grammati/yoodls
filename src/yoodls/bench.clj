(ns yoodls.bench)

(defn bench-fn [f]
  (let [time-f (fn [n]
                 (let [start (System/nanoTime)]
                   (dotimes [_ n]
                     (f))
                   (- (System/nanoTime) start)))
        devnul (proxy [java.io.Writer] []
                 (write [])
                 (flush []))]
                                        ;(binding [*out* devnul])
    (loop [n 1]
      (let [t (time-f n)]
        (if (< t 1e8)
          (recur (* 2 n))
          (double ( / t n)))))))

(defmacro bench [& body]
  `(let [ns# (bench-fn (fn [] ~@body))
         [n# u#] (cond
                  (> ns# 1e9) [(/ ns# 1e9) "s"]
                  (> ns# 1e6) [(/ ns# 1e6) "ms"]
                  (> ns# 1e3) [(/ ns# 1e3) "us"]
                  :else       [ns# "ns"])]
     (println (format "%g %s" n# u#))))

