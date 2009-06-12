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
  (:use cynojure.util)
  (:use cynojure.cl)
  (:use clojure.contrib.str-utils)
  (:use clojure.contrib.sql))

(defmacro definfix [op]
  `(defn ~(symbol (str 'sql- op)) [& exprs#]
     (interpose '~op exprs#)))

(definfix and)
(definfix or)
(definfix like)
(definfix =)
(definfix >)
(definfix >=)
(definfix <)
(definfix <=)

(defn- sql-list [& exprs]
  (str-join "," exprs))

(defn- sql-list* [exprs]
  (apply sql-list (mklist exprs)))

(defn- sql-concat [& exprs]
  (str-join " " exprs))

(defn- sql-pairs [& exprs]
  (str-join "," (map (fn [x] (apply sql-concat x)) exprs)))

(defn- sql-pairs* [exprs]
  (apply sql-pairs (mklist exprs)))

(defn sql-in [var match-list]
  (sql-concat var 'in (sql-list match-list)))

;; (sql-in 'foo '(1 2 3))

(defn emit [expr]
  (cond
    (string? expr) (printf "'%s'" expr)
    (seq? expr) (do
                  (print "(")
                  (foreach emit (interpose \space expr))
                  (print ")"))
    :else (print expr)))

(defun sql [sexpr]
  "Return the specified s-expression in its equivalent SQL form.

  (sql (sql-and (sql->= 'age 20) (sql-like 'name \"%smith%\"))) =>
  \"((age >= 20) and (name like '%smith%'))\""
  (with-out-str (emit sexpr)))

(defun query-str [what :key from where order-by group-by limit offset]
  "Return a SQL SELECT query string, for the specified args.

  (query-str \"count(1)\"
             :from 'person
             :where (sql-and (sql->= 'age 20)
                             (sql-<= 'age 40))
             :order-by '((name asc) (id desc)))"
  (with-out-str
    (print "select" (sql-list* what)
           "from" (sql-list* from))
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

(defn- xform-sym [sym hmap]
  (let [xform (fn [s] (apply str (replace hmap s)))]
    (if (keyword? sym)
      (keyword (xform (name sym)))
      (symbol (xform (name sym))))))

(defun lispy-sym [sym] (xform-sym sym {\_ \-}))
(defun sqly-sym [sym] (xform-sym sym {\- \_}))
;; (sqly-sym (lispy-sym :foo_bar))

(defun insert-object [table obj]
  "Add `obj' to `table' using the clojure.contrib.sql/insert-values
  function, after converting Lispy hyphen separated symbols in the
  keys of `obj' to SQL friendly underscore separated ones,
  i.e. `:foo-bar' becomes `:foo_bar'."
  (insert-values (sqly-sym table) (map sqly-sym (keys obj)) (vals obj)))
