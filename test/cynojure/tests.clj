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

(deftest test-cl
  (is (= (ignore-errors (+ 1 2)) 3))
  (is (= (ignore-errors (/ 5 0)) nil))
  (is (let [tm 3356647006]
        (= (date-to-universal-time (universal-time-to-date tm)) tm))))

(deftest test-util
  (is (= (tostr :foo) "foo"))
  (is (= (mklist 'a) '(a)))
  (is (= (mklist '(a)) '(a)))
  (is (= (ip-to-dotted (dotted-to-ip "127.0.0.1")) "127.0.0.1"))
  (is (= (md5-sum "foobar") "3858f62230ac3c915f300c664312c63f"))
  (is (not (nil? (get-system-classpaths))))

  (is (= (csv-escape "foo's, goal") "\"foo''s, goal\""))
  (is (not (nil? (uuid))))
  (is (= (parse-int "123") 123))
  (is (nil? (parse-int "@bar")))
  (is (= (.toString (parse-date "Tue, 09 Jun 2009 22:11:40 GMT")) "Tue Jun 09 15:11:40 PDT 2009"))
  (is (= (.toString (parse-sql-date "Tue, 09 Jun 2009 22:11:40 GMT")) "2009-06-09"))
  (is (= (.toString (parse-sql-timestamp "Tue, 09 Jun 2009 22:11:09 GMT")) "2009-06-09 15:11:09.0"))
  (is (= (:host (parse-url "http://www.google.com/")) "www.google.com"))
  (is (= (str-trim "abc123" 3) "abc"))
  (is (= (str-trim "abc" 5) "abc"))

  (is (= (string= "foobar" "FooBar") true))
  (is (= (char-numeric? \5) true))
  (is (= (char-numeric? \a) false))
  )

(deftest test-sql
  (is (= (sql [1 2 3]) "(1,2,3)"))
  (is (= (sql "foo'bar") "'foo''bar'"))
  (is (= (sql [:as :age :foo]) "age AS foo"))
  (is (= (sql [:in :age [20 25 30]]) "(age IN (20,25,30))"))
  (is (= (sql [:and [:>= :age 20] [:like :name "%smith%"]])
         "((age >= 20) AND (name LIKE '%smith%'))"))
  (is (= (sql [:and [:>= :age 20] [:= :name "O'Henry"]])
         "((age >= 20) AND (name = 'O''Henry'))"))
  (is (= (query-str [:count 1]
                    :from :person
                    :where [:and [:>= :age 20] [:<= :age 40] [:like :first-name "%smith%"]]
                    :order-by '((:name :asc) (:id :desc)))         
         "SELECT COUNT(1) FROM person WHERE ((age >= 20) AND (age <= 40) AND (first_name LIKE '%smith%')) ORDER BY name asc,id desc"))
  (is (= (query-str [:as [:count 1] :cnt]
                    :from :person
                    :where [:>= :age 20])
         "SELECT COUNT(1) AS cnt FROM person WHERE (age >= 20)"))
  (is (= (query-str '(:name :age :sex)
		    :from :person
		    :where [:and [:>= :age 20] [:<= :age 40]]
		    :order-by '((:name :asc)))
         "SELECT name,age,sex FROM person WHERE ((age >= 20) AND (age <= 40)) ORDER BY name asc"))
  (is (= (update-str 'person
                     {:name "Alice", :age 30},
                     :where [:= :id 123])
         "UPDATE person SET name=?,age=? WHERE (id = 123)"))
  (is (= (create-temp-table :report-p-daily-d
                            '((:p-id :bigint)
                              (:d :integer :default 0))
                            :on-commit :drop)
         "CREATE TEMP TABLE report_p_daily_d ( p_id bigint,d integer default 0 ) ON COMMIT drop"))
  (is (= (create-temp-table :report-p-daily-l1
                            '((:p-id :bigint)
                              (:ip-address :bigint)
                              (:l1 :integer :default 0))
                            :on-commit :drop)
         "CREATE TEMP TABLE report_p_daily_l1 ( p_id bigint,ip_address bigint,l1 integer default 0 ) ON COMMIT drop"))
  (is (= (insert-into :report-p-daily-d
                      '(:p-id :d)
                      (select '(:p-id [:count 1])
                              :from :audit-log-entry
                              :where [:and
                                      [:= :o-id 1]
                                      [:>= :ctime 3468470400]
                                      [:< :ctime 3468556800]]
                              :group-by :p-id))
         "INSERT INTO report_p_daily_d (p_id,d) (SELECT p_id,COUNT(1) FROM audit_log_entry WHERE ((o_id = 1) AND (ctime >= 3468470400) AND (ctime < 3468556800)) GROUP BY p_id)"))
  (is (= (insert-into :report-p-daily-l2
                      '(:p-id :l2)
                      (select '(:p-id [:count 1])
                              :from :audit-log-entry
                              :where [:and
                                      [:= :o-id 1]
                                      [:>= :ctime 3468470400]
                                      [:< :ctime 3468556800]
                                      [:or [:like :user-agent "Mozilla%"] [:like :user-agent "Opera%"]]]
                              :group-by :p-id))
         "INSERT INTO report_p_daily_l2 (p_id,l2) (SELECT p_id,COUNT(1) FROM audit_log_entry WHERE ((o_id = 1) AND (ctime >= 3468470400) AND (ctime < 3468556800) AND ((user_agent LIKE 'Mozilla%') OR (user_agent LIKE 'Opera%'))) GROUP BY p_id)"))
  (is (= (insert-into :report-p-daily-s
                      '(:p-id :s)
                      (select '(:foo.p-id [:count 1])
                              :from (as (select '(:p-id :ip-address)
                                                :from :audit-log-entry
                                                :where [:and
                                                        [:= :o-id 1]
                                                        [:>= :ctime 3468470400]
                                                        [:< :ctime 3468556800]
                                                        [:not [:or [:like :user-agent "Mozilla%"] [:like :user-agent "Opera%"]]]]
                                                :group-by '(:p-id :ip-address))
                                        :foo)
                              :group-by :foo.p-id))
         "INSERT INTO report_p_daily_s (p_id,s) (SELECT foo.p_id,COUNT(1) FROM (SELECT p_id,ip_address FROM audit_log_entry WHERE ((o_id = 1) AND (ctime >= 3468470400) AND (ctime < 3468556800) AND NOT(((user_agent LIKE 'Mozilla%') OR (user_agent LIKE 'Opera%')))) GROUP BY p_id,ip_address) as foo GROUP BY foo.p_id)"))
  (is (= (insert-into :report-p-daily
                      '(:ctime :p-id :d :s :l2 :l1)
                      (select '(3468470400
                                :report-p-daily-d.p-id
                                :report-p-daily-d.d
                                :report-p-daily-s.s
                                :report-p-daily-l2.l2
                                :report-p-daily-l1.l1)
                              :from :report-p-daily-d
                              :full-outer-join '((:report-p-daily-s
                                                  [:= :report-p-daily-d.p-id :report-p-daily-s.p-id])
                                                 (:report-p-daily-l2
                                                  [:= :report-p-daily-d.p-id :report-p-daily-l2.p-id])
                                                 (:report-p-daily-l1
                                                  [:= :report-p-daily-d.p-id :report-p-daily-l1.p-id]))))
         "INSERT INTO report_p_daily (ctime,p_id,d,s,l2,l1) (SELECT 3468470400,report_p_daily_d.p_id,report_p_daily_d.d,report_p_daily_s.s,report_p_daily_l2.l2,report_p_daily_l1.l1 FROM report_p_daily_d FULL OUTER JOIN report_p_daily_s ON (report_p_daily_d.p_id = report_p_daily_s.p_id) FULL OUTER JOIN report_p_daily_l2 ON (report_p_daily_d.p_id = report_p_daily_l2.p_id) FULL OUTER JOIN report_p_daily_l1 ON (report_p_daily_d.p_id = report_p_daily_l1.p_id))"))

  (is (= (insert-into :actor
                      '(:firstname :surname :password)
                      [["firstname" "surname" "password"]
                       ["firstname" "surname" "password"]]
                      :returning [:a :b])
         "INSERT INTO actor (firstname,surname,password) ('firstname','surname','password'), ('firstname','surname','password') RETURNING (a,b)"))
  )

;; (run-tests 'cynojure.tests)
