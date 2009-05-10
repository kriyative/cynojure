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
(definfix =)
(definfix >=)
(definfix <)

(defn- sql-list [& exprs]
  (str-join "," exprs))

(defn- sql-concat [& exprs]
  (str-join " " exprs))

(defn sql-in [var match-list]
  (sql-concat var 'in (sql-list match-list)))

;; (sql-in 'foo '(1 2 3))

(defun sql-select-stmt [what :key from where order-by group-by limit offset]
  "Return a SQL SELECT query string, for the specified args."
  (apply sql-concat
	 "select"
	 (apply sql-list (mklist what))
	 "from" (apply sql-list (mklist from))
	 `(~@(and where `("where" ~where))
	   ~@(and order-by `("order by" ~(apply sql-list (mklist order-by))))
	   ~@(and group-by `("group by" ~(apply sql-list (mklist group-by))))
	   ~@(and limit `("limit" ~limit))
	   ~@(and offset `("offset" ~offset)))))

(defun sql-count [:key from where]
  "Perform SELECT COUNT(1) using the specified FROM and WHERE
  clauses."
  (with-query-results rs
      (sql-select-stmt "count(1)" :from from :where where)
    (:count (first rs))))

(defun sql-update-stmt [table value-map :key where]
  "Return a SQL UPDATE command string using the specified args."
  (apply sql-concat
	 `("update" ~table
	   "set" ~(apply sql-list (map (fn [x] (str (tostr x) "=?")) (keys value-map)))
	   "where" ~where)))

(defun sql-update [table value-map :key where]
  "Perform a SQL UPDATE command using the specified args."
  (apply do-prepared (sql-update-stmt table value-map :where where) [(vals value-map)]))

(defun create-sequence [seq-name]
  "Create a sequence called seq-name."
  (do-commands
   (format "create sequence %s" (tostr seq-name))))

(defun sequence-next [seq-name]
  "Get next val of the specified sequence"
  (with-query-results rs (format "select nextval('%s')" (tostr seq-name))
    (:nextval (first rs))))
