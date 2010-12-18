;; aws.clj -- amazon aws helper functions

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2009. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; this package essentially wraps the jets3t
;; (https://jets3t.dev.java.net/) and typica
;; (http://code.google.com/p/typica/) java libraries. It should be
;; noted that typica has a number of prerequisite java libraries,
;; which are assumed to be available somewhere in the classpath. Also,
;; typica requires the JAXB (https://jaxb.dev.java.net/) library,
;; which in turn has some additional requirements under Java 5. The
;; following code has only been tested with Java 6. If you must run
;; under Java 5, see:
;;
;; http://groups.google.com/group/typica/browse_thread/thread/7c4ec97a5de40471/b1918b79b4fb99dd
;;
;; for a description of the issues, and possible solutions.

(ns cynojure.aws
  (:import (java.io FileInputStream)
	   (org.jets3t.service.security AWSCredentials)
	   (org.jets3t.service.impl.rest.httpclient RestS3Service)
	   (org.jets3t.service.model S3Object)
	   (org.jets3t.service.utils Mimetypes)
	   (org.jets3t.service.utils ServiceUtils)
	   (com.xerox.amazonws.sqs2 SQSUtils)
	   (com.xerox.amazonws.sqs2 MessageQueue)
	   (com.xerox.amazonws.sqs2 Message))
  (:use [clojure.contrib.duck-streams :only [file-str]])
  (:use cynojure.cl cynojure.util clojure.contrib.java-utils))

(def *default-aws-authfile* (user-homedir-path ".awskeys.clj"))
(def *default-aws-profile* nil)

(defun load-aws-auth [:key
                      [profile *default-aws-profile*]
                      [authfile *default-aws-authfile*]]
  "Load a specified AWS authentication `profile' from an
`authfile' (default ~/.awskeys.clj). The awskeys.clj is of the
following format:

 {\"profile1\" [\"ACCESS-KEY\" \"SECRET-KEY\"] ...}

where \"profile1\" can be any symbolic name passed in to the
load-aws-auth function."
  (let [auth-map (read (sexp-reader (file-str authfile)))]
    (auth-map (or profile *default-aws-authfile*))))

(def *aws-access-key* nil)
(def *aws-secret-key* nil)

(defmacro with-aws-profile
  "See documentation of function `load-aws-auth'."
  [[profile] & body]
  `(binding [*default-aws-profile* ~profile]
     (let [foo# (load-aws-auth)]
       (binding [*aws-access-key* (first foo#)
                 *aws-secret-key* (second foo#)]
         ~@body))))

(def *s3* nil)

(defn s3-get-service [& [access-key secret-key]]
  "Return a S3 request service using the credentials specified in
  `access-key' and `secret-key'."
  (new RestS3Service (new AWSCredentials
                          (or access-key *aws-access-key*)
                          (or secret-key *aws-secret-key*))))

(defun s3-create-bucket [name :key [s *s3*]]
  "Create a new bucket named `name'."
  (.createBucket s name))

(defun s3-get-bucket [name :key [s *s3*]]
  "Find a bucket named `name'."
  (.getBucket s name))

(defun s3-list-objects [bucket path :key [s *s3*]]
  (.listObjects s bucket path "/" (long 1024)))

(defun s3-put-file [bucket path source :key content-type acl [s *s3*]]
  "Put a file in the specified `bucket' and `path' on S3. The arg
  `source' specifies the local file, and `content-type', and `acl' can
  optionally override the defaults."
  (let [obj (new S3Object path)
	f (file source)]
    (.setBucketName obj bucket)
    (.setContentLength obj (.length f))
    (.setContentType obj (or content-type (.getMimetype (Mimetypes/getInstance) f)))
    (.setDataInputFile obj f)
    (.setMd5Hash obj (ServiceUtils/computeMD5Hash (new FileInputStream f)))
    (when acl (.addMetadata obj "x-amz-acl" acl))
    (.putObject s bucket obj)))

(defun s3-delete-object [bucket object-key :key [s *s3*]]
  "Delete a specified object identified by `object-key' in `bucket'."
  (.deleteObject s bucket object-key))

(defmacro with-s3 [s & body]
  "Create a S3 service binding, and invoke `body'."
  `(binding [*s3* ~s]
     ~@body))

(defmacro with-s3-service [[& [s]] & body]
  "Create a S3 service binding, and invoke `body'."
  `(binding [*s3* (or ~s (s3-get-service))]
     ~@body))

;;;;;;;;;;;;;;;; typica

(defun sqs-get-queue [access-key secret-key qname :key [encoding? false]]
  "Return a SQS queue service using the credentials specified in
  `access-key' and `secret-key'."
  (let [q (com.xerox.amazonws.sqs2.SQSUtils/connectToQueue
           qname
           (or access-key *aws-access-key*)
           (or secret-key *aws-secret-key*))]
    (.setEncoding q encoding?)
    q))

(defun sqs-get-service [:key [access-key *aws-access-key*]
                        [secret-key *aws-secret-key*]
                        [secure? true]
                        [server "queue.amazonaws.com"]]
  (new com.xerox.amazonws.sqs2.QueueService access-key secret-key secure? server))

(def *sqs* nil)
(defmacro with-sqs-service [[& sqs-get-service-args] & body]
  `(binding [*sqs* (sqs-get-service ~@sqs-get-service-args)]
     ~@body))

(defun sqs-get-message-queue [qname :key [s *sqs*]] (.getMessageQueue s qname))
(defun sqs-get-or-create-message-queue [qname :key [s *sqs*]]
  (.getOrCreateMessageQueue s qname))

(def *sqs-queue* nil)

(defun sqs-receive-message [:key [queue *sqs-queue*]]
  "Retrieve one message from `queue'."
  (.receiveMessage queue))

(defun sqs-receive-messages [count :key [queue *sqs-queue*]]
  "Retrieve multiple messages from `queue'. The arg `count' specifies
  how many messages to retrieve."
  (.receiveMessages queue (int count)))

(defun sqs-delete-message [m :key [queue *sqs-queue*]]
  "Delete the specified message `m' from `queue'."
  (.deleteMessage queue m))

(defun sqs-send-message [msg :key [queue *sqs-queue*]]
  "Send specified msg to `queue'."
  (.sendMessage queue (if (string? msg) msg (.toString msg))))

(defun sqs-message-body [m]
  "Get the message body of `m'."
  (.getMessageBody m))

(defun sqs-get-message-count [:key [queue *sqs-queue*]]
  "Get the count of messages in `queue'."
  (.getApproximateNumberOfMessages queue))

(defmacro with-sqs-queue [queue & body]
  "Create a SQS queue binding, and invoke `body'."
  `(binding [*sqs-queue* ~queue]
     ~@body))
