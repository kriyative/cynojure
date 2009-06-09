;; util.clj -- clojure utility functions

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2009. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cynojure.util
  (:use clojure.contrib.seq-utils)
  (:import (java.security MessageDigest)
	   (java.io ByteArrayInputStream))
  (:use cynojure.cl))

(defun tostr [arg]
  "Return a usable string representation of the specified arg."
  (cond
   (keyword? arg) (name arg)
   true (str arg)))

;; (tostr :foo)
;; (tostr 'foo)

(defun mklist [arg]
  "Return arg if it's a list, otherwise return a new list containing
arg. e.g,

  (mklist 'a) => '(a)
  (mklist '(a)) => '(a)"
  (if (list? arg) arg (list arg)))

(defun dotted-to-ip [dotted]
  "Convert a dotted notation IPv4 address string to a 32-bit integer. e.g.,

   (dotted-to-ip \"127.0.0.1\") => 2130706433"
  (let [[[b1 _] [b2 _] [b3 _] [b4 _]] (re-seq #"([0-9]+)" dotted)]
    (bit-or (bit-or (bit-or (bit-shift-left (new Integer b1) 24)
                            (bit-shift-left (new Integer b2) 16))
                    (bit-shift-left (new Integer b3) 8))
            (new Integer b4))))

;; (dotted-to-ip "127.0.0.1")

(defun ip-to-dotted [ip]
  "Convert a 32-bit integer into a dotted notation IPv4 address string. e.g.,

  (ip-to-dotted (dotted-to-ip \"127.0.0.1\")) => \"127.0.0.1\""
  (format "%d.%d.%d.%d"
          (bit-and (bit-shift-right ip 24) 0xff)
          (bit-and (bit-shift-right ip 16) 0xff)
          (bit-and (bit-shift-right ip 8) 0xff)
          (bit-and ip 0xff)))

;; (ip-to-dotted (dotted-to-ip "127.0.0.1"))

(defun md5-sum [message]
  "Generate a MD5 checksum of the specified message."
  (let [digest (MessageDigest/getInstance "MD5")]
    (.update digest (.getBytes message))
    (let [byte-arr (.digest digest)
          bigint (new BigInteger 1 byte-arr)
          bigint-str (.toString bigint 16)
          leading-zeros (apply str (replicate (- 32 (count bigint-str)) \0))]
      (str leading-zeros bigint-str))))

;; (md5-sum "foobar")

(defun get-classpaths []
  "Return the list of classpath entries."
  (seq (.getURLs (clojure.lang.RT/baseLoader))))

;; (get-classpaths)

(defun get-system-classpaths []
  "Return the list of system classpath entries."
  (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))

;; (get-system-classpaths)

(defun foreach [f seq]
  "A basic foreach, when you absolutely need to side effect on a
  sequence.

  (foreach print '(a b c 1 2 3)) => nil
  abc123"
  (when-not (empty? seq)
    (f (first seq))
    (recur f (rest seq))))

;; (foreach print '(a b c))

(defun csv-escape [s]
  "Escape the specified string per CSV rules.

  (csv-escape \"foo's, goal\") =>
  \"\\\"foo''s, goal\\\"\""
  (let [quotep (includes? s \,)]
    (apply str
	   (concat
	    (if quotep "\"" "")
	    (replace {\" "\"\"" \' "''"} s)
	    (if quotep "\"" "")))))

;; (csv-escape "foo's, goal")

(defun uuid []
  (.toString (java.util.UUID/randomUUID)))

;; (uuid)
