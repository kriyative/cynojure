;; sql.clj -- s-sexpression based sql query interface for clojure

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2009. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cynojure.sql
  (:use clojure.contrib.str-utils clojure.contrib.sql
	clojure.contrib.sql.internal clojure.contrib.except
	clojure.contrib.fcase)
  (:use cynojure.util cynojure.cl))

(defn- sym-xform [s hmap]
  (let [subst (fn [s] (apply str (replace hmap s)))]
    (cond
      (symbol? s) (symbol (subst (name s)))
      (keyword? s) (keyword (subst (name s)))
      :else (subst s))))

(defun lispy [s] (sym-xform s {\_ \-}))
(defun sqly [s] (sym-xform s {\- \_}))

(defun sql-str [s]
  (cond
    (or (symbol? s) (keyword? s)) (sqly (name s))
    (string? s) (str "'" s "'")
    :else s))

(defn- sql-list [& exprs]
  (str-join "," exprs))

(defn- sql-list* [exprs]
  (apply sql-list (mklist exprs)))

(defn- sql-pairs [& exprs]
  (str-join "," (map (fn [[k v]] (str (sql-str k) " " (sql-str v))) exprs)))

(defn- sql-pairs* [exprs]
  (apply sql-pairs (mklist exprs)))

(def *sql-expr-handlers*
     {:count (fn [op args]
	       (print "count(")
	       (print (first args))
	       (print ")"))
      :as (fn [op [ex alias]]
	    (sql-emit ex)
	    (print " as" (sql-str alias)))
      :in (fn [op [c l]]
	    (print "(")
	    (print (sql-str c) (str "in (" (sql-list* l) ")"))
	    (print ")"))})

(defn- sql-emit-expr [[op & args]]
  (if-let [handler (*sql-expr-handlers* op)]
      (handler op args)
    (do
      (print "(")
      (sql-emit (first args))
      (doseq [arg (rest args)]
	(print (str " " (sql-str op) " "))
	(sql-emit arg))
      (print ")"))))

(defn- sql-emit-list [x]
  (sql-emit (first x))
  (doseq [e (rest x)]
    (print ",")
    (sql-emit e)))

(defun sql-emit [x]
  (cond
    (vector? x) (sql-emit-expr x)
    (list? x) (sql-emit-list x)
    (= x :all) (print "*")
    :else (print (sql-str x))))

(defun sql [expr]
  (with-out-str (sql-emit expr)))

(defun query-str [what :key from where order-by group-by limit offset]
  "Return a SQL SELECT query string, for the specified args.

  (query-str \"count(1)\"
             :from 'person
             :where (sql-and (sql->= 'age 20)
                             (sql-<= 'age 40))
             :order-by '((name asc) (id desc)))"
  (with-out-str
    (print "select" (sql what)
           "from" (sql from))
    (when where (print " where" (if (string? where) where (sql where))))
    (when order-by (print " order by" (sql-pairs* order-by)))
    (when group-by (print " group by" (sql-list* group-by)))
    (when limit (print " limit" limit))
    (when offset (print " offset" offset))))

(defun count-rows [:key from where]
  "Perform SELECT COUNT(1) using the specified FROM and WHERE
  clauses."
  (with-query-results rs
      [(query-str "count(1) as count" :from from :where where)]
    (:count (first rs))))

(defun update-str [table value-map :key where]
  "Return a SQL UPDATE command string using the specified args.

  (update-str 'person
              {:name \"Alice\", :age 30},
              :where (sql-= 'id 123))"
  (with-out-str
    (print "update" table
           "set" (str-join "," (map (fn [x] (str (tostr x) "=?")) (keys value-map)))
           "where" (if (string? where) where (sql where)))))

(defun update [table value-map :key where]
  "Perform a SQL UPDATE command using the specified args."
  (apply do-prepared (update-str table value-map :where where) [(vals value-map)]))

(defun create-sequence [seq-name]
  "Create a sequence called seq-name."
  (do-commands
   (format "create sequence %s" (tostr seq-name))))

(defun sequence-next [seq-name]
  "Get next val of the specified sequence"
  (with-query-results rs
      [(format "select nextval('%s')" (tostr seq-name))]
    (:nextval (first rs))))

(defun insert-object [table obj]
  "Add `obj' to `table' using the clojure.contrib.sql/insert-values
  function, after converting Lispy hyphen separated symbols in the
  keys of `obj' to SQL friendly underscore separated ones,
  i.e. `:foo-bar' becomes `:foo_bar'."
  (insert-values (sqly table) (map sqly (keys obj)) (vals obj)))

;; lifted from clojure/core.clj -- call lispy on the column names
;; before making keywords out of them; this ensures they look more
;; lispy with hyphen instead of underscore separators
(defn lispy-resultset-seq
  "Creates and returns a lazy sequence of structmaps corresponding to
  the rows in the java.sql.ResultSet rs"
  [rs]
  (let [rsmeta (. rs (getMetaData))
	idxs (range 1 (inc (. rsmeta (getColumnCount))))
	keys (map (comp keyword lispy #(.toLowerCase #^String %))
		  (map (fn [i] (. rsmeta (getColumnLabel i))) idxs))
	check-keys (or (apply distinct? keys)
		       (throw (Exception. "ResultSet must have unique column labels")))
	row-struct (apply create-struct keys)
	row-values (fn [] (map (fn [#^Integer i] (. rs (getObject i))) idxs))
	rows (fn thisfn []
	       (lazy-seq
		 (when (. rs (next))
		   (cons (apply struct row-struct (row-values)) (thisfn)))))]
    (rows)))

;; based on with-query-results* from clojure.contrib/sql/internal.clj
;; -- override resultset-sql with lispy-resultset-seq
(defn perform-with-resultset
  "Executes a query, then evaluates func passing in a seq of the results as
  an argument. The first argument is a vector containing the (optionally
  parameterized) sql query string followed by values for any parameters."
  [sql func]
  (with-open [stmt (.prepareStatement (connection*) sql)]
    (with-open [rset (.executeQuery stmt)]
      (func (lispy-resultset-seq rset)))))

(defmacro with-resultset
  "Executes a query, then evaluates body with results bound to a seq of the
  results. sql-params is a vector containing a string providing
  the (optionally parameterized) SQL query followed by values for any
  parameters."
  [results sql-params & body]
  `(perform-with-resultset ~sql-params (fn [~results] ~@body)))
