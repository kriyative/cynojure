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
  (:use clojure.contrib.java-utils)
  (:use [clojure.contrib.duck-streams :only [reader]])
  (:use [clojure.contrib.seq-utils :only [includes?]])
  (:import (java.security MessageDigest)
	   (java.io ByteArrayInputStream))
  (:use cynojure.cl))

(defun member [x sq :key [test =]]
  (let [member1 (fn [x sq]
                  (when (seq sq)
                    (if (test x (first sq))
                      sq
                      (recur x (rest sq)))))]
    (member1 x sq)))

;; (member 1 '(2 3 4 5 1))

(defun str* [& args]
  "Like `str', return a concatenated string of all the args, except
convert keyword symbols to strings without a leading colon
character. i.e., (str* :foo) => \"foo\""
  (apply str (map #(if (instance? clojure.lang.Named %) (name %) (str %)) args)))

(defun str*-join [sep coll]
  "Like clojure.contrib/str-join, concatenates the `coll' using the
`str*' function and delimit each element with `sep'."
  (apply str* (interpose sep coll)))

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
  (if (seq? arg) arg (list arg)))

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
  "Generate a MD5 hash of the specified message."
  (let [digest (MessageDigest/getInstance "MD5")]
    (.update digest (.getBytes message))
    (let [bigint-str (.toString (new BigInteger 1 (.digest digest)) 16)
          leading-zeros (apply str (replicate (- 32 (count bigint-str)) \0))]
      (str leading-zeros bigint-str))))

;; (md5-sum "foobar")"3858f62230ac3c915f300c664312c63f"

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
  "Return a UUID string."
  (.toString (java.util.UUID/randomUUID)))

;; (uuid)

(defun parse-int [s :key error]
  "Parse the string `s' as an integer. By default any parse errors are
  suppressed, but if `error' is true, a java.lang.NumberFormatException
  will be raised."
  (if error
    (Integer/parseInt s)
    (try
     (Integer/parseInt s)
     (catch java.lang.NumberFormatException _ nil))))

;; (parse-int "@5a")
;; (parse-int "@5a" :error true)

(defun parse-double [s :key error]
  "Parse the string `s' as a double precision floating point
  number. By default any parse errors are suppressed, but if `error'
  is true, a java.lang.NumberFormatException will be raised."
  (if error
    (Double/parseDouble s)
    (try
     (Double/parseDouble s)
     (catch java.lang.NumberFormatException _ nil))))

(def *current-timezone* nil)
(defun parse-date [date-str :key format timezone]
  "Parse date-str using the optional format, and timezone into a
java.util.Date object."
  (let [parser (new java.text.SimpleDateFormat
                    (or format "EEE, d MMM yyyy HH:mm:ss Z"))
        timezone (or timezone *current-timezone*)]
    (if timezone (.setTimeZone parser timezone))
    (.parse parser date-str)))

;; (parse-date "Tue, 09 Jun 2009 22:11:40 GMT")

(defun parse-sql-date [date-str & parse-date-args]
  "Parse date-str into a java.sql.Date object using the parse-date
function. The `parse-date-args' are additional args to pass to the
parse-date function."
  (let [date (apply parse-date date-str parse-date-args)]
    (new java.sql.Date (.getTime date))))

;; (parse-sql-date "Tue, 09 Jun 2009 22:11:40 GMT")

(defun parse-sql-timestamp [date-str & parse-date-args]
  "Parse date-str into a java.sql.Timestamp object using the
parse-date function. The `parse-date-args' are additional args to pass
to the parse-date function."
  (let [date (apply parse-date date-str parse-date-args)]
    (new java.sql.Timestamp (.getTime date))))

;;  (parse-sql-timestamp "Tue, 09 Jun 2009 22:11:09 GMT")

(defmulti to-date class)

(defmethod to-date java.util.Date [d] d)
(defmethod to-date java.sql.Date [d] (new java.util.Date (.getTime d)))
(defmethod to-date java.sql.Timestamp [d] (new java.util.Date (.getTime d)))

(defun format-date [date :key format timezone]
  (let [date (to-date date)
        formatter (new java.text.SimpleDateFormat
                       (or format "EEE, d MMM yyyy HH:mm:ss Z"))
        timezone (or timezone *current-timezone*)]
    (if timezone (.setTimeZone formatter timezone))
    (.format formatter date)))

;; (format-date (new java.util.Date))
;; (format-date (new java.sql.Date) :format )

(defn format-iso8601-date [date] (format-date date :format "yyyy-MM-dd'T'HH:mm:ssZ"))

(defn now [] (new java.util.Date))

(def url-re
     #"([a-z]+)://([-a-zA-Z0-9_.]+)(:[0-9]+)*[/]*([^#]*)(#\w+)*(\?.*)*")

;; (re-seq url-re "http://google.com/foo/bar/biz#frag1?x=1&y=2")
;; (re-seq url-re "http://google.com/foo/bar/biz")
;; (["http://google.com/foo/bar/biz" "http" "google.com" nil "foo/bar/biz" nil nil])

(defun parse-url [url]
  "Parse the specified url, and return a map of its components."
  (let [[_ scheme host port path frag query] (first (re-seq url-re url))]
    {:scheme scheme
     :host host
     :port port
     :path path
     :fragment frag
     :query query}))

;; (parse-url "http://google.com/foo/bar/biz")

(defun str-trim [s n]
  "Trim the string `s' to a length of no more than `n' characters."
  (if (string? s) (subs s 0 (min (count s) n)) s))

(defun perform-with-retry [fn :key [retries 5] sleep no-error]
  (loop [i 1]
    (let [[r arg] (try
		   [:ok (fn)]
		   (catch Exception e [:error e]))]
      (if (= r :ok)
	arg
	(if (< i retries)
	  (do
	    (if sleep (Thread/sleep sleep))
	    (recur (+ i 1)))
	  (when-not no-error (throw arg)))))))

(defmacro with-retry [[& args] & body]
  `(perform-with-retry (fn [] ~@body) ~@args))

(comment
  (perform-with-retry (fn [] (/ 5 0)) :retries 3)
  
  (with-retry []
    (/ 5 0))
  (with-retry [:retries 8 :no-error true]
    (/ 5 0))
  )

(defun sexp-reader [source]
  "Wrap `source' in a reader suitable to pass to `read'."
  (new java.io.PushbackReader (reader source)))

(defun user-homedir-path [& [file-name]]
  "Return the home dir path, with the optional file-name appended."
  (str (java.lang.System/getProperty "user.home")
       (java.lang.System/getProperty "file.separator")
       file-name))

;; (user-homedir-path)

(defun file-name-non-directory [path]
  "Return the non-directory (filename only) portion of `path'."
  (.getName (file path)))

;; (file-name-non-directory "/Users/foo/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *log-level* (ref {:info false :debug false :warn true :error true}))

(defn logger [level fmt & args]
  (when (get @*log-level* level)
    (apply printf (str "%s [%6s] " fmt)
           (format-date (new java.util.Date) :format "yyyy-MM-dd HH:mm:ss")
           (name level)
           args)
    (flush)))

(defn log-enable [level]
  (dosync (ref-set *log-level* (assoc @*log-level* level true))))

;; (log-enable :info)
;; @*log-level*

(defn log-disable [level]
  (dosync (ref-set *log-level* (dissoc @*log-level* level))))
;; (log-disable :info)

(defun string-input-stream [s]
  (new java.io.StringBufferInputStream s))

(defun string= [s1 s2]
  "Returns boolean result of case insensitive comparison of strings s1
and s2. e.g.,

  (string= \"foobar\" \"FooBar\") => true"
  (= (.toLowerCase s1) (.toLowerCase s2)))

(defun string-upcase [s] (.toUpperCase s))
(defun string-downcase [s] (.toLowerCase s))

(defun char-numeric? [c] (<= (int \0) (int c) (int \9)))
