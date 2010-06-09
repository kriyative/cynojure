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
  (:use clojure.contrib.str-utils clojure.contrib.java-utils
        clojure.contrib.sql clojure.contrib.sql.internal
        clojure.contrib.except clojure.contrib.fcase)
  (:use cynojure.util cynojure.cl))

(defn- sym-xform [s hmap]
  (let [subst (fn [s] (apply str (replace hmap s)))]
    (cond
      (symbol? s) (symbol (subst (name s)))
      (keyword? s) (keyword (subst (name s)))
      (string? s) (subst s)
      :else s)))

(defun lispy [s] (sym-xform s {\_ \-}))
(defun sqly [s] (tostr (sym-xform s {\- \_})))

(defun sql-str-escape [s]
  (with-out-str
    (loop [s s]
      (let [ch (first s)]
        (when-not (empty? s)
          (print ch)
          (cond
            (= ch \') (let [ch2 (first (rest s))]
                        (cond
                          (or (nil? ch2) (not (= ch2 \'))) (print \')
                          ch2 (print ch2))
                        (recur (rest s)))
            true (recur (rest s))))))))

;; (sql-str-escape "foo'bar'")

(defun sql-str [s]
  (cond
    (or (symbol? s) (keyword? s)) (sqly (name s))
    (string? s) (str "'" (sql-str-escape s) "'")
    :else s))

;; (sql-str "foo'bar")

(defn- sql-pairs [& exprs]
  (str-join ","
            (map (fn [[k & v]]
                   (str-join " " (cons (sql-str k) (map sql-str v)))) exprs)))
(defn- sql-pairs* [exprs]
  (apply sql-pairs (mklist exprs)))

(defun safely-first [x] (if (or (seq? x) (vector? x)) (first x) x))

(defmulti emit "Emit a SQL expression tree" safely-first)

(defmethod emit :as [[_ expr alias]]
  (emit expr)
  (print " AS ")
  (emit alias))

(defmethod emit :null? [[_ col]]
  (emit col)
  (print " IS NULL"))

(defmethod emit :not-null? [[_ col]]
  (emit col)
  (print " IS NOT NULL"))

(defn emit-function [[op & args]]
  (print (str (string-upcase (name op)) "("))
  (emit (first args))
  (doseq [arg (rest args)]
    (print ",")
    (emit arg))
  (print ")"))

(defmacro defsqlfun [name]
  `(defmethod emit ~name [& args#] (apply emit-function args#)))

(defsqlfun :count)
(defsqlfun :sum)
(defsqlfun :round)
(defsqlfun :not)
(defsqlfun :nextval)

(defn emit-infix [[op & args]]
  (print "(")
  (emit (first args))
  (doseq [arg (rest args)]
    (print " ")
    (print (string-upcase (name op)))
    (print " ")
    (emit arg))
  (print ")"))

(defmacro defsqlinfix [name]
  `(defmethod emit ~name [& args#] (apply emit-infix args#)))

(defsqlinfix :and)
(defsqlinfix :or)
(defsqlinfix :<)
(defsqlinfix :<=)
(defsqlinfix :>)
(defsqlinfix :>=)
(defsqlinfix :=)
(defsqlinfix :<>)
(defsqlinfix :like)

(defmethod emit :in [[_ c l]]
  (print "(")
  (emit c)
  (print " IN ")
  (emit l)
  (print ")"))

(defun emit-list [x]
  (print "(")
  (emit (first x))
  (doseq [e (rest x)]
    (print ",")
    (emit e))
  (print ")"))

(defmethod emit :default [expr]
  (cond
    (vector? expr) (emit-list expr)
    (list? expr) (emit-list expr)
    (= expr :all) (print "*")
    :else (print (sql-str expr))))

(defun sql [expr]
  (with-out-str (emit expr)))

;;;;;;;;;;;;;;;;

(defun query-str [what :key from where order-by group-by limit offset full-outer-join]
  "Return a SQL SELECT query string, for the specified args.

  (query-str [:as [:count 1] :cnt]
             :from :person
             :where [:and [:>= :age 20] [:<= :age 40]])
   => \"select count(1) as cnt from person where ((age >= 20) and (age <= 40))\""
  (with-out-str
    (print "SELECT" (sql what)
           "FROM" (if (string? from) from (sql from)))
    (when where (print " WHERE" (if (string? where) where (sql where))))
    (when full-outer-join
      (doseq [[join-table on-clause] full-outer-join]
        (print " FULL OUTER JOIN" (sql-str join-table) "ON ")
        (emit on-clause)))
    (when group-by (print " GROUP BY" (sql group-by)))
    (when order-by (print " ORDER BY" (sql-pairs* order-by)))
    (when limit (print " LIMIT" limit))
    (when offset (print " OFFSET" offset))))

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
    (print "UPDATE" (sqly table)
           "SET"
           (str-join "," (map (fn [x] (str (sqly (name x)) "=?")) (keys value-map)))
           "WHERE" (if (string? where) where (sql where)))))

(defun update [table value-map :key where]
  "Perform a SQL UPDATE command using the specified args."
  (apply do-prepared (update-str table value-map :where where) [(vals value-map)]))

(defun create-sequence [seq-name]
  "Create a sequence called seq-name."
  (format "CREATE SEQUENCE %s" (tostr seq-name)))

(defun drop-sequence [seq-name]
  (format "DROP SEQUENCE IF EXISTS %s" (tostr seq-name)))

(defun sequence-next [seq-name]
  "Get next val of the specified sequence"
  (with-query-results rs
      [(format "SELECT NEXTVAL('%s')" (tostr seq-name))]
    (:nextval (first rs))))

;; lifted from clojure/core.clj -- call lispy on the column names
;; before making keywords out of them; this ensures they look more
;; lispy with hyphen instead of underscore separators
(defn lispy-resultset-seq
  "Creates and returns a lazy sequence of structmaps corresponding to
  the rows in the java.sql.ResultSet rs"
  [rs]
  (let [rsmeta (.getMetaData rs)
	idxs (range 1 (inc (.getColumnCount rsmeta)))
	keys (map (comp keyword lispy #(.toLowerCase #^String %))
		  (map (fn [i] (.getColumnLabel rsmeta i)) idxs))
	check-keys (or (apply distinct? keys)
		       (throw (Exception. "ResultSet must have unique column labels")))
	row-struct (apply create-struct keys)
	row-values (fn [] (map (fn [#^Integer i] (.getObject rs i)) idxs))
	rows (fn thisfn []
	       (lazy-seq
		 (when (.next rs)
		   (cons (apply struct row-struct (row-values)) (thisfn)))))]
    (rows)))

(defun do-insert-get-pkeys [sql param-group]
  (with-open [stmt (.prepareStatement (connection) sql)]
    (doseq [[index value] (map vector (iterate inc 1) param-group)]
      (.setObject stmt index value))
    (transaction
     (.execute stmt)
     (if (and (< 0 (.getUpdateCount stmt)) (.getMoreResults stmt))
       (let [rs (.getResultSet stmt)]
         (loop [pkeys []]
           (if (.next rs)
             (recur (conj pkeys (.getObject rs 1)))
             pkeys)))))))

(defun insert-values-get-pkeys [table column-names value-group :key sequence-name]
  "Derived from clojure.contrib.sql/insert-values, returns a ResultSet
seq of the auto generated pkeys in this insert."
  (let [column-strs (map as-str column-names)
        n (count value-group)
        template (apply str (interpose "," (replicate n "?")))
        columns (if (seq column-names)
                  (format "(%s)" (apply str (interpose "," column-strs)))
                  "")]
    (do-insert-get-pkeys (str (format "INSERT INTO %s %s VALUES (%s)"
                                      (as-str table) columns template)
                              (if sequence-name
                                (format "; SELECT CURRVAL('%s')" (tostr sequence-name))
                                ""))
                         value-group)))

(defun insert-object [table obj :key sequence-name]
  "Add `obj' to `table' using the clojure.contrib.sql/insert-values
  function, after converting Lispy hyphen separated symbols in the
  keys of `obj' to SQL friendly underscore separated ones,
  i.e. `:foo-bar' becomes `:foo_bar'."
  (insert-values-get-pkeys (sqly table) (map sqly (keys obj)) (vals obj)
                           :sequence-name sequence-name))

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

(defun create-table* [table-name cols & postscript]
  (str-join " "
            ["CREATE TABLE"
             (sql-str table-name)
             "("
             (str-join ","
                       (map (fn [[k & v]]
                              (str-join " " (cons (sql k) (map sql v))))
                            cols))
             (if postscript (str "," (str-join "," postscript)) "")
             ")"]))

(defun drop-table* [table-name]
  (str-join " " ["DROP TABLE IF EXISTS" (sql-str table-name)]))

(defun create-temp-table [table-name cols :key on-commit]
  (str-join " "
            ["CREATE TEMP TABLE"
             (sql-str table-name)
             "(" (sql-pairs* cols) ")"
             (if on-commit (str "ON COMMIT " (sql-str on-commit)) "")]))

(defun select [& args] (apply query-str args))
(defun as [expr alias] (str "(" expr ") as " (sql-str alias)))

(defun select1 [& query-args]
  (with-resultset rs
    (apply query-str query-args)
    (first rs)))

(defun insert-into [table-name cols what]
  (str-join " "
            ["INSERT INTO"
             (sql-str table-name)
             (with-out-str (emit-list cols))
             (str "(" what ")")]))

(defun create-index [name table cols]
  (str-join " "
            ["CREATE INDEX"
             (sql-str name)
             "ON"
             (sql-str table)
             (with-out-str (emit-list cols))]))

(defun drop-index [name]
  (str-join " " ["DROP INDEX" "IF EXISTS" (sql-str name) ]))
