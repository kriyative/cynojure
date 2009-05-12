;; tests.clj -- unit tests for cynojure

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2009. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cynojure.tests
  (:use clojure.contrib.sql)
  (:use clojure.contrib.test-is)
  (:use cynojure.cl)
  (:use cynojure.util)
  (:use cynojure.sql))

(deftest test-util
  (is (= (tostr :foo) "foo"))
  (is (= (mklist 'a) '(a)))
  (is (= (mklist '(a)) '(a)))
  (is (= (ip-to-dotted (dotted-to-ip "127.0.0.1")) "127.0.0.1"))
  (is (= (md5-sum "foobar") "3858f62230ac3c915f300c664312c63f"))
  (is (not (nil? (get-classpaths))))
  (is (not (nil? (get-system-classpaths)))))

(deftest test-sql
  (is (= (sql (sql-and (sql->= 'age 20) (sql-like 'name "%smith%")))
	 "((age >= 20) and (name like '%smith%'))"
	 ))
  (is (= (query-str "count(1)"
		    :from 'person
		    :where (sql-and (sql->= 'age 20) (sql-<= 'age 40))
		    :order-by '((name asc) (id desc)))
	 "select count(1) from person where ((age >= 20) and (age <= 40)) order by name asc,id desc"))
  (is (= (update-str 'person
		     {:name "Alice", :age 30},
		     :where (sql-= 'id 123))
	 "update person set name=?,age=? where (id = 123)"))
  (is (pr-str (sql-and (sql->= 'age 20) (sql-<= 'age "foo")))
      "((age >= 20) and (age <= 40))"))

;; (run-tests 'cynojure.tests)
