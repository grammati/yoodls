(ns yoodls.bench)

(defn bench-fn
  [f]
  (let [noop   (fn [] nil)
        time-n-reps
               (fn [f n]
                 (let [start (System/nanoTime)]
                   (dotimes [_ n]
                     (f))
                   (- (System/nanoTime) start)))
        timer  (fn [f n]
                 (let [t (- (time-n-reps f n)
                            (time-n-reps noop n))]
                   (if (neg? t) 0 t)))
        devnul (proxy [java.io.Writer] []
                 (write [_])
                 (flush []))]
    (binding [*out* devnul]
      (System/gc)
      (let [rep-count (loop [n 1]
                        (let [t (timer f n)]
                          (if (or (> t 1e8) (> n 1e9))
                            n
                            (recur (* 2 n)))))
            timeit (fn []
                     (/ (double (timer f rep-count)) rep-count))]
        ;; warm-up the JIT
        (dotimes [_ 5] (f))
        (System/gc) 
        (first (sort (take 3 (repeatedly timeit))))))))

(defn nanos-to-string [nanos]
  (let [[n unit] (cond
                  (> nanos 1e9) [(/ nanos 1e9) "s"]
                  (> nanos 1e6) [(/ nanos 1e6) "ms"]
                  (> nanos 1e3) [(/ nanos 1e3) "us"]
                  :else       [nanos "ns"])]
    (format "%g %s" n unit)))

(defmacro bench
  "Measure and print the execution time of the body."
  [& body]
  `(println (nanos-to-string (bench-fn (fn [] ~@body)))))

