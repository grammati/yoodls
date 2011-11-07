(ns yoodls.pipe
  (import (java.util.concurrent ArrayBlockingQueue LinkedBlockingQueue)))


(defn pipe
  "Returns a `pipe`, as [put s], where put is a function that will put
  items into the pipe, and s is a (potentially blocking) seq of the
  items in the pipe.  The optional parameter is a map of
  options. Supported options are:
  :capacity - The capacity of the underlying LinkedBlockingQueue.
              Default: 256 (TBD)
  :sentinel - A value that signals the end of the seq.
              Default: nil
  "
  ;; TODO - Write a benchmark to choose the "best" default for
  ;; capacity, and for whether to use Array or Linked blocking queue.
  [& [{:keys [capacity sentinel]
       :or {capacity 256 sentinel nil}}]]
  (let [q   (LinkedBlockingQueue. (int capacity))
        NIL (Object.)                   ; LBQ cannot handle real nils
        put (fn [o] (.put q (if (nil? o) NIL o)))
        pop (fn [] (let [o (.take q)]
                     (if (identical? o NIL) nil o)))
        s   (take-while (partial not= sentinel) (repeatedly pop))]
    [put s]))


;;; Experimental stuff

(defn pipe-map [[put s] f]
  [put (map f s)])

(defn throwing-pipe
  "Same as pipe, but can throw exceptions to the consumer.
   Takes an addional option, :error?, a function that will determine
   whether an item extracted from the pipe signals an error. The
   default is to treat any instance of Throwable as an error. The
   intent is that producers should signal an error by catching an
   Exception and putting it into the pipe."
  [& [{:keys [error?] :as opts}]]
  (let [error?  (or error? #(instance? Throwable %))
        rethrow #(if (error? %) (throw %) %)]
    (pipe-map rethrow (pipe (dissoc opts :error?)))))

