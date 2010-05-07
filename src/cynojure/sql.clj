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
      (string? s) (subst s)
      :else s)))

(defun lispy [s] (sym-xform s {\_ \-}))
(defun sqly [s] (sym-xform s {\- \_}))

(defun sql-str [s]
  (cond
    (or (symbol? s) (keyword? s)) (sqly (name s))
    (string? s) (str "'" s "'")
    :else s))

(defn- sql-list [& exprs]
  (str-join "," (map sql-str exprs)))

(defn- sql-list* [exprs]
  (apply sql-list (mklist exprs)))

(defn- sql-pairs [& exprs]
  (str-join "," (map (fn [[k & v]] (str-join " " (cons (sql-str k) (map sql-str v)))) exprs)))

(defn- sql-pairs* [exprs]
  (apply sql-pairs (mklist exprs)))

;;;;;;;;;;;;;;;;

(defn render-form-function [[op & args]]
  (print (str (name op) "("))
  (render (first args))
  (doseq [arg (rest args)]
    (print ",")
    (render arg))
  (print ")"))

(defmacro defsqlfun [name]
  `(defmethod render-form ~name [& args#] (apply render-form-function args#)))

(defn render-form-mathop [[op arg1 arg2]]
  (print "(" (sqly arg1) (sql-str op) (sqly arg2) ")"))

(defmacro defsqlmathop [name]
  `(defmethod render-form ~name [& args#] (apply render-form-mathop args#)))

(defmulti render-form "Render a SQL expression tree." {:private true} first)

(defmethod render-form :as [[_ expr alias]]
  (render-form expr)
  (print " as" (sql-str alias)))

(defmethod render-form :null? [[_ col]]
  (print (sql-str col) "is null"))

(defmethod render-form :not-null? [[_ col]]
  (print (sql-str col) "is not null"))

(defsqlfun :count)
(defsqlfun :sum)
(defsqlfun :round)
(defsqlfun :not)

(defmethod render-form :in [_ [c l]]
  (print "(" (sql-str c) (str "in (" (sql-list* l) ")")))

(declare render)

(defmethod render-form :default [[op & args]]
  (print "(")
  (render (first args))
  (doseq [arg (rest args)]
    (print (str " " (sql-str op) " "))
    (render arg))
  (print ")"))

(defn- render-list [x]
  (render (first x))
  (doseq [e (rest x)]
    (print ",")
    (render e)))

(defun render [x]
  (cond
    (vector? x) (render-form x)
    (list? x) (render-list x)
    (= x :all) (print "*")
    :else (print (sql-str x))))

(defun sql [expr]
  (with-out-str (render expr)))

;;;;;;;;;;;;;;;;

(defun query-str [what :key from where order-by group-by limit offset full-outer-join]
  "Return a SQL SELECT query string, for the specified args.

  (query-str [:as [:count 1] :cnt]
             :from :person
             :where [:and [:>= :age 20] [:<= :age 40]])
   => \"select count(1) as cnt from person where ((age >= 20) and (age <= 40))\""
  (with-out-str
    (print "select" (sql what)
           "from" (if (string? from) from (sql from)))
    (when where (print " where" (if (string? where) where (sql where))))
    (when full-outer-join
      (doseq [[join-table on-clause] full-outer-join]
        (print " full outer join" (sql-str join-table) "on ")
        (render on-clause)))
    (when group-by (print " group by" (sql group-by)))
    (when order-by (print " order by" (sql-pairs* order-by)))
    (when limit (print " limit" limit))
    (when offset (print " offset" offset))))

(defun count-rows [:key from where]
  "Perform SELECT COUNT(1) using the specified FROM and WHERE
  clauses."
  (with-query-results rs
      [(query-str [:as [:count 1] :count] :from from :where where)]
    (:count (first rs))))

(defun update-str [table value-map :key where]
  "Return a SQL UPDATE command string using the specified args.

  (update-str :person {:name \"Alice\", :age 30} :where [:= :id 123])
  => \"update :person set name=?,age=? where (id = 123)\""
  (with-out-str
    (print "update" (sqly table)
           "set" (str-join "," (map (fn [x] (str (sqly (name x)) "=?")) (keys value-map)))
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

(defun create-temp-table [table-name cols :key on-commit]
  (str-join " "
            ["create temp table"
             (sql-str table-name)
             "(" (sql-pairs* cols) ")"
             (if on-commit (str "on commit " (sql-str on-commit)) "")]))

(defun select [& args] (apply query-str args))
(defun as [expr alias] (str "(" expr ") as " (sql-str alias)))

(defun insert-into [table-name cols what]
  (str-join " "
            ["insert into"
             (sql-str table-name)
             (str "(" (sql-list* cols) ")")
             (str "(" what ")")]))
