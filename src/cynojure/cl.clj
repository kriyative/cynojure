;; cl.clj -- common lisp like interface for clojure

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2009. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cynojure.cl)

;; defun based on
;; http://groups.google.com/group/clojure/msg/51bb53ca077154f8
(defmacro defun
  "Common Lisp style defun. Define functions with optional keyword args. e.g.,

  (defun foo [a :key b [c 10]] (list a b c))

  (foo 1 :b 2) => (1 2 10)"
  [sym args & body]
  (let [[pargs [_ & kargs]] (split-with (fn [x] (not (= x :key))) args)
	gkeys (gensym "gkeys__")
	letk (fn [k]
	       (let [[nm val] (if (vector? k) k [k])
		     kname (keyword (name nm))]
		 `(~nm (if (contains? ~gkeys ~kname) (~gkeys ~kname) ~val))))
	doc (if (string? (first body)) (first body) "")
	body (if (string? (first body)) (rest body) body)]
    (if kargs
      `(defn ~sym
	 {:arglists '([~@pargs :key ~@kargs])
	  :doc ~doc}
	 [~@pargs & k#]
	 (let [~gkeys (apply hash-map k#)
	       ~@(apply concat (map letk kargs))]
	   ~@body))
      `(defn ~sym ~doc [~@pargs] ~@body))))

(defonce +unix-epoch+ 2208988800)

(defun universal-time-to-date [universal-time]
  "Convert a Common Lisp universal-time value to a java.util.Date
  object."
  (new java.util.Date (* (- universal-time +unix-epoch+) 1000)))

;; (universal-time-to-date 3356647006)

(defun date-to-universal-time [& [date]]
  "Convert a java.util.Date to a Common Lisp universal-time value."
  (+ +unix-epoch+ (int (/ (.getTime (or date (new java.util.Date))) 1000))))

;; (date-to-universal-time (universal-time-to-date 3356647006))
;; (let [tm 3356647006] (= (date-to-universal-time (universal-time-to-date tm)) tm))

(defmacro ignore-errors [& body]
  "Evaluate `body' and return its value, or catch any exception, which
may be thrown, and return nil."
  `(try
    (do ~@body)
    (catch Exception ex#
      nil)))
