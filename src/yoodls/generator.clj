(ns yoodls.generator
  (:use (yoodls (pipe :only [pipe]))))

;; Idea: sometimes creating a lazy sequence in clojure seems
;; hard. Python generators, by comparison, seem easy. Maybe it's just
;; me. This is an attempt to make (fake?) a Python-style yield
;; statement in Clojure.
;; 
;; Fibonacci numbers are probably the simplest example, although not
;; particularly motivating, as there are already elegant solutions in
;; clojure. Nevertheless:
;;
;; def fib():
;;   a,b = 1,1
;;   while True:
;;     yield a
;;     a,b = b,a+b
;;
;; In Python, a function that contains a yield statement produces an
;; iterator. Each invocation of yield can be thought of as an
;; imperative push of a value to the consumer of the sequence.
;; What if we had a yield statement in Clojure? It might look like this:
(comment
  (defn fib []
    (loop [a 1 b 1]
      (yield a)
      (recur b (+ a b)))))

;; Here's another example - the real use case that motivated me to try
;; this.  In parsing XML with SAX, imagine that we have a sequence of
;; "XML events" - objects of the form [type data] - such as:
;; ([:start-element :foo] [:text "hello"] [:text "&"] [:text "blah"]
;; ... [:end-element :foo]). We want to transform that sequence into
;; one where all adjacent :text events have been merged into one.
;;
;; Here is a solution in Python:
;; 
;; def merge_text(events):
;;   text = ''
;;   for e in events:
;;     type,data = e
;;     if type == 'text':
;;       text += data
;;     else:
;;       if text:
;;         yield ['text' text]
;;         text = ''
;;       yield e
;; 
;; Of course, there are idiomatic, functional solutions in
;; Clojure. Here is one (the one I ended up using):
(comment
  (defn merge-text [events]
    (letfn [(text? [[type _]]
              (= :text type))
            (merge-if-text [[e & _ :as  evts]]
              (if (text? e)
                (list [:text (apply str (map second evts))])
                evts))]
    (->> events
         (partition-by text?)
         (map merge-if-text)
         (apply concat)))))

;; But what if we could do it like this instead?
(comment
  (defgenerator merge-text [events]
    (loop [[[type data :as e] & events] events
           text nil]
      (when e
        (if (= :text type)
          (recur events (str text data))
          (do
            (when text
              (yield [:text text]))
            (yield e)
            (recur events nil)))))))

;; Or even like this?
(comment
  (defgenerator merge-text [events]
    (let [text (atom nil)]
      (doseq [[[type data :as e]] events]
        (if (= :text type)
          (swap! text str data)
          (do
            (when @text
              (yield [:text @text])
              (reset! text nil))
            (yield e)))))))

;; Is it simpler? Not really, but it's potentially easier for people
;; who are used to the imperative-yield style of producing lazy
;; sequences.


;; It turns out that Python generators can be faked pretty easily.
;; This solution uses the pipe function from yoodls.pipe, which is
;; basically just a thin wrapper over a blocking queue. We run the
;; body of the generator in a thread, binding the symbol 'yield' to a
;; function that puts an item into the queue. We return the "other
;; end" of the pipe - a seq that pulls items off the queue. Easy
;; peasy. As a bonus, for a limited time, I'm throwing in a yield*,
;; which will yield each of the elements in another seq. Suck on that,
;; GVR! ;)
;; Of course, there are some differences from Python generators.
;; 1) In Python, exceptions that occur in the body of the generator
;; get thrown to the consumer of the output sequence, when he calls
;; 'next' (usually implicitly, via a for loop). Can we do that here?
;; I'm not sure, but for now I'm just putting the exception in the
;; queue :P
;; 2) Python generators have "send", and some other fancy
;; crap. Whatev.

(defmacro generator [arglist & body]
  `(fn ~arglist
     (let [end-of-seq#  (Object.)
           [~'yield s#] (pipe {:sentinel end-of-seq#})
           ~'yield* #(doseq [i# %] (~'yield i#))]
       (future
         (try
           ~@body
           (catch Throwable e#
             (~'yield e#))
           (finally
            (~'yield end-of-seq#))))
       s#)))

(defmacro defgenerator [name arglist & body]
  `(def ~name (generator ~arglist ~@body)))


