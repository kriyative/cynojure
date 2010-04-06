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
  (:use cynojure.cl clojure.contrib.java-utils))

(def *s3* nil)

(defn s3-get-service [access-key secret-key]
  "Return a S3 request service using the credentials specified in
  `access-key' and `secret-key'."
  (let [aws-cred (new AWSCredentials access-key secret-key)]
    (new RestS3Service aws-cred)))

(defun s3-create-bucket [name :key [s *s3*]]
  "Create a new bucket named `name'."
  (.createBucket s name))

(defun s3-get-bucket [name :key [s *s3*]]
  "Find a bucket named `name'."
  (.getBucket s name))

(defun s3-list-objects [bucket path :key [s *s3*]]
  (.listObjects s bucket path "/" 1024))

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

(defmacro with-s3 [s & body]
  "Create a S3 service binding, and invoke `body'."
  `(binding [*s3* ~s]
     ~@body))

;;;;;;;;;;;;;;;; typica

(defun sqs-get-queue [access-key secret-key qname :key [encoding? false]]
  "Return a SQS queue service using the credentials specified in
  `access-key' and `secret-key'."
  (let [q (com.xerox.amazonws.sqs2.SQSUtils/connectToQueue qname access-key secret-key)]
    (.setEncoding q encoding?)
    q))

(def *sqs-queue* nil)

(defun sqs-receive-message [:key [queue *sqs-queue*]]
  "Retrieve one message from `queue'."
  (.receiveMessage queue))

(defun sqs-receive-messages [count :key [queue *sqs-queue*]]
  "Retrieve multiple messages from `queue'. The arg `count' specifies
  how many messages to retrieve."
  (.receiveMessages queue count))

(defun sqs-delete-message [m :key [queue *sqs-queue*]]
  "Delete the specified message `m' from `queue'."
  (.deleteMessage queue m))

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
