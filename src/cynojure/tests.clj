(ns cynojure.tests
  (:use clojure.contrib.sql)
  (:use cynojure.cl)
  (:use cynojure.util)
  (:use cynojure.sql))

(comment
  (= (tostr :foo) "foo")
  (= (mklist '(a)) '(a))
  (= (dotted-to-ip "127.0.0.1") 2130706433)
  (= (ip-to-dotted (dotted-to-ip "127.0.0.1")) "127.0.0.1")
  (= (md5-sum "foobar") "3858f62230ac3c915f300c664312c63f")
  (not (nil? (get-classpaths)))
  (not (nil? (get-system-classpaths)))

  (sql-select-stmt "count(1)"
		   :from 'person
		   :where (sql-and (sql->= 'age 20)
				   (sql-<= 'age 40))
		   :order-by '((name asc) (id desc)))

  (sql-update-stmt 'person
		   {:name "Alice", :age 30},
		   :where (sql-= 'id 123))

  )
