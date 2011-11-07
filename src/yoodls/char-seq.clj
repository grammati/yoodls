(ns yoodls.char-seq
  (:import (java.nio CharBuffer))
  (:require (clojure.java [io :as io])))


#_(defn char-seq
  ([^Reader in]
     (char-seq (CharBuffer/allocate 0x100) 0))
  ([^Reader in ^CharBuffer buf ^Long offset]
     (let [in]
       (reify
         CharSequence
         (charAt [this length]
           (.charAt buf (+ length offset))
         (length [this]
           nil)
         (subSequence [this start end]
           nil)
         (toString [this]
            nil))))))

(defprotocol PersistentReader
  "An immutable variant of Java's Reader interface. Reading from a
   given instance will always return the same characters."
  (read-str [this n] ; "read" and "read-string" are spoken for
    "Returns a String of the first n characters.")
  (skip [this n]
    "Returns a new PersistentReader advanced by n characters"))

;;; Implementations of PersitentReader
;;; * wrap a Reader:

(defn perisistent-reader [in])

(deftype CharSeq [^Reader in
                  ^:volatile-mutable ^chars buf
                  ^long offset]
  CharSequence
  (charAt [this length]
    (when (> length offset)
      (set! buf (char-array 1024)))
    (aget buf length))
  (length [this]
    nil)
  (subSequence [this]
    nil)
  (toString [this]
    nil)

  PersistentReader
  )

(defn char-seq [in]
  (let [in (io/reader in)]
    (CharSeq. in nil 0)))



(defn test-charseq []
  (let [cs-1 (char-seq (java.io.StringReader. "foobar"))]
    (is (= \f (.charAt cs-1 0)))
    (let [[s cs-2] (chop-at 3 cs-1)]
      (is (= \f (.charAt cs-1 0)))
      (is (= \b (.charAt cs-2 0)))
      (is (= \r (.charAt cs-2 2)))
      (is (= \r (.charAt cs-1 5))))))







