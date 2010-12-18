cynojure
========

cynojure is a library of utilities for Clojure. It has a dependency on
[clojure-contrib](http://code.google.com/p/clojure-contrib/). You can
read more about its design and goals at the [cynojure
blog](http://cynojure.posterous.com).

License
=======

Eclipse Public License - v 1.0.

Quickstart
==========

cynojure.cl
-----------

A few "old habits die hard" things from Common-Lisp, including a
`defun' macro, which supports keyword args in the lambda-list and
conversion from unix-epoch java.util.Dates to universal-time and back.

cynojure.util
-------------

An assortment of utility functions, most notably some lispy wrappers
on Java library classes and methods. The functions are somewhat well
documented in docstrings.

cynojure.aws
------------

A set of helper functions to connect to Amazon AWS services. In order
to call the functions in cynojure.aws, you'll need to configure AWS
credentials using one of the following methods:

Configuring AWS credentials:

    (binding [*aws-access-key* "<your-access-key>"
              *aws-secret-key* "<your-secret-key>"]
      ;; call AWS functions
      )

or alternatively if you'd like to keep your AWS credentials out of
source files, you can create a ~/.awskeys.clj file, which must
contain a map such as the following:

    {"profile1" ["profile1-aws-access-key" "profile1-aws-secret-key"]
     "profile2" ["profile2-aws-access-key" "profile2-aws-secret-key"]}

Now, in your Clojure source you can reference the above profiles by
name:

    (with-aws-profile ["profile1"]
      ;; call AWS functions
      )

Working with S3:

    (use cynojure.aws)

    ;; Get a list of objects in a S3 bucket, which match a specific
    ;; path prefix

    (with-aws-profile ["profile1"]
      (with-s3-service []
        (map (fn [o]
               {:key (.getKey o)
                :content-length (.getContentLength o)
                :content-type (.getContentType o)
                :last-modified (.getLastModifiedDate o)})
             (s3-list-objects "bucket" "prefix/"))))

    ;; Put a local file into a S3 bucket

    (with-aws-profile ["profile1"]
      (with-s3-service []
        (s3-put-file "bucket"
                     "test/path/file.txt"
                     "/path/to/local/file"
                     :content-type "text/plain"
                     :acl "public-read")))

    ;; Delete an object from S3

    (with-aws-profile ["profile1"]
      (with-s3-service []
        (s3-delete-object "bucket" "test/path/file.txt")))

Working with SQS:

    ;; Get the count of pending messages in a SQS queue

    (with-aws-profile ["profile1"]
      (with-sqs-service []
        (sqs-get-message-count :queue (sqs-get-or-create-message-queue "qname"))))

    ;; Receive and delete messages

    (with-aws-profile ["profile1"]
      (with-sqs-service []
        (with-sqs-queue (sqs-get-or-create-message-queue "qname")
          (doseq [msg (sqs-receive-messages 10)]
            (println (sqs-message-body msg))
            (sqs-delete-message msg)))))

cynojure.sql
------------

A Lispy way to write SQL queries. The macros in this package allow you
to use Lispy names with hyphens, which are automatically converted to
underscore characters in the equivalent SQL.

e.g.,

    (use 'cynojure.sql)

    (select [:count 1]
            :from :person
            :where [:and [:>= :age 20] [:<= :age 40] [:like :first-name "%smith%"]]
            :order-by '((:name :asc) (:id :desc)))
    > "SELECT COUNT(1) FROM person WHERE ((age >= 20) AND (age <= 40) AND (first_name LIKE '%smith%')) ORDER BY name asc,id desc"

    ;; using select in a database connection

    (with-connection *db*
      (with-resultset rs
        (select '(:id :first-name :age)
                :from :person
                :where [:and [:>= :age 20] [:<= :age 40] [:like :first-name "%smith%"]]
                :order-by '((:name :asc) (:id :desc)))
        (doall rs)))
