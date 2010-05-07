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
  (is (not (nil? (get-classpaths))))
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
  (is (= (str-trim "abc" 5) "abc")))

(deftest test-sql
  (is (= (sql [:and [:>= :age 20] [:like :name "%smith%"]])
         "((age >= 20) and (name like '%smith%'))"))
  (is (= (query-str [:count 1]
                    :from :person
                    :where [:and [:>= :age 20] [:<= :age 40] [:like :first-name "%smith%"]]
                    :order-by '((:name :asc) (:id :desc)))
	 "select count(1) from person where ((age >= 20) and (age <= 40) and (first_name like '%smith%')) order by name asc,id desc"))
  (is (= (query-str [:as [:count 1] :cnt]
                    :from :person
                    :where [:>= :age 20])
	 "select count(1) as cnt from person where (age >= 20)"))
  (is (= (query-str '(:name :age :sex)
		    :from :person
		    :where [:and [:>= :age 20] [:<= :age 40]]
		    :order-by '((:name :asc)))
	 "select name,age,sex from person where ((age >= 20) and (age <= 40)) order by name asc"))
  (is (= (update-str 'person
                     {:name "Alice", :age 30},
                     :where [:= :id 123])
         "update person set name=?,age=? where (id = 123)"))
  (is (= (create-temp-table :report-p-daily-d
                            '((:p-id :bigint)
                              (:d :integer :default 0))
                            :on-commit :drop)
         "create temp table report_p_daily_d ( p_id bigint,d integer default 0 ) on commit drop"))
  (is (= (create-temp-table :report-p-daily-l1
                            '((:p-id :bigint)
                              (:ip-address :bigint)
                              (:l1 :integer :default 0))
                            :on-commit :drop)

         "create temp table report_p_daily_l1 ( p_id bigint,ip_address bigint,l1 integer default 0 ) on commit drop"))
  (is (= (insert-into :report-p-daily-d
                      '(:p-id :d)
                      (select '(:p-id [:count 1])
                              :from :audit-log-entry
                              :where [:and
                                      [:= :o-id 1]
                                      [:>= :ctime 3468470400]
                                      [:< :ctime 3468556800]]
                              :group-by :p-id))
         "insert into report_p_daily_d (p_id,d) (select p_id,count(1) from audit_log_entry where ((o_id = 1) and (ctime >= 3468470400) and (ctime < 3468556800)) group by p_id)"))
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
         "insert into report_p_daily_l2 (p_id,l2) (select p_id,count(1) from audit_log_entry where ((o_id = 1) and (ctime >= 3468470400) and (ctime < 3468556800) and ((user_agent like 'Mozilla%') or (user_agent like 'Opera%'))) group by p_id)"))

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
         "insert into report_p_daily_s (p_id,s) (select foo.p_id,count(1) from (select p_id,ip_address from audit_log_entry where ((o_id = 1) and (ctime >= 3468470400) and (ctime < 3468556800) and not(((user_agent like 'Mozilla%') or (user_agent like 'Opera%')))) group by p_id,ip_address) as foo group by foo.p_id)"))

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
         "insert into report_p_daily (ctime,p_id,d,s,l2,l1) (select 3468470400,report_p_daily_d.p_id,report_p_daily_d.d,report_p_daily_s.s,report_p_daily_l2.l2,report_p_daily_l1.l1 from report_p_daily_d full outer join report_p_daily_s on (report_p_daily_d.p_id = report_p_daily_s.p_id) full outer join report_p_daily_l2 on (report_p_daily_d.p_id = report_p_daily_l2.p_id) full outer join report_p_daily_l1 on (report_p_daily_d.p_id = report_p_daily_l1.p_id))"))  
  )

;; (run-tests 'cynojure.tests)
