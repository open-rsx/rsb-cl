;;;; util.lisp ---
;;;;
;;;; Copyright (C) 2012, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.serialization)

;;; Keywords

(declaim
 (ftype (function (local-time:timestamp) non-negative-integer)
        timestamp->unix/microseconds)
 (ftype (function (non-negative-integer) local-time:timestamp)
        unix/microseconds->timestamp)
 (inline timestamp->unix/microseconds unix/microseconds->timestamp))

(defun timestamp->unix/microseconds (timestamp)
  "Convert the `local-time:timestamp' instance TIMESTAMP into an
   integer which counts the number of microseconds since UNIX epoch."
  (declare #.+optimization-fast+unsafe+)

  (+ (* 1000000 (the non-negative-integer
                     (local-time:timestamp-to-unix timestamp)))
     (* 1       (the non-negative-integer
                     (local-time:timestamp-microsecond timestamp)))))

(defun unix/microseconds->timestamp (unix-microseconds)
  "Convert UNIX-MICROSECONDS to an instance of
   `local-time:timestamp'."
  (declare #.cl-rsb-system::+optimization-fast+unsafe+)

  (let+ (((&values unix-seconds microseconds)
          (floor unix-microseconds 1000000)))
    (local-time:unix-to-timestamp
     unix-seconds :nsec (* 1000 microseconds))))

(declaim (inline string->bytes bytes->string))

(defun string->bytes (string)
  "Converter STRING into an octet-vector."
  (declare (notinline string->bytes))
  (sb-ext:string-to-octets string :external-format :ascii))

(defun bytes->string (bytes)
  "Convert BYTES into a string."
  (sb-ext:octets-to-string bytes :external-format :ascii))

;;; Keywords

(defconstant +keyword-readtable+
  (if (boundp '+keyword-readtable+)
      (symbol-value '+keyword-readtable+)
      (let ((readtable (copy-readtable nil)))
        (setf (readtable-case readtable) :invert)
        readtable))
  "This readtable is used to print and read keywords. The goal is to
   get a natural mapping between Lisp keywords and corresponding
   strings for most cases.")

(declaim
 (ftype (function (keyword)      octet-vector) keyword->bytes/no-cache)
 (ftype (function (octet-vector) keyword)      bytes->keyword/no-cache)
 (inline keyword->bytes/no-cache bytes->keyword/no-cache))

(defun keyword->bytes/no-cache (keyword)
  "Convert the name of KEYWORD into an octet-vector."
  (declare #.+optimization-fast+unsafe+)

  (if (find #\: (symbol-name keyword))
      (string->bytes (symbol-name keyword))
      (let ((*readtable* +keyword-readtable+))
        (string->bytes (princ-to-string keyword)))))

(defun bytes->keyword/no-cache (bytes)
  "Converter BYTES into a keyword."
  (declare #.+optimization-fast+unsafe+)

  (if (find (char-code #\:) bytes)
      (intern (bytes->string bytes) (find-package :keyword))
      (let ((*package*   (find-package :keyword))
	    (*readtable* +keyword-readtable+))
	(read-from-string (bytes->string bytes)))))

(declaim (special *keyword->bytes-cache* *bytes->keyword-cache*))

(defun make-keyword->bytes-cache ()
  (make-hash-table :test #'eq))

(defvar *keyword->bytes-cache* (make-keyword->bytes-cache)
  "Cache for keyword -> bytes conversion.

   Should be rebound thread-locally since there is concurrency control
   is not performed.")

(defun make-bytes->keyword-cache ()
  (make-hash-table :test #'equal))

(defvar *bytes->keyword-cache* (make-bytes->keyword-cache)
  "Cache for bytes -> keyword conversion.

   Should be rebound thread-locally since there is concurrency control
   is not performed.")

(declaim (inline wire-schema->bytes bytes->wire-schema))

(defun keyword->bytes (keyword)
  "Convert the name of KEYWORD into an octet-vector."
  (declare #.+optimization-fast+unsafe+)

  (ensure-gethash keyword *keyword->bytes-cache*
                  (keyword->bytes/no-cache keyword)))

(defun bytes->keyword (bytes)
  "Converter BYTES into a keyword."
  (declare #.+optimization-fast+unsafe+)

  (ensure-gethash bytes *bytes->keyword-cache*
                  (bytes->keyword/no-cache bytes)))

;;; Wire-schemas

(declaim (special *wire-schema->bytes-cache* *bytes->wire-schema-cache*))

(defun make-wire-schema->bytes-cache ()
  (make-hash-table :test #'eq))

(defvar *wire-schema->bytes-cache* (make-wire-schema->bytes-cache)
  "Cache for wire-schema -> bytes conversion.

   Should be rebound thread-locally since there is concurrency control
   is not performed.")

(defun make-bytes->wire-schema-cache ()
  (make-hash-table :test #'equalp))

(defvar *bytes->wire-schema-cache* (make-bytes->wire-schema-cache)
  "Cache for bytes -> wire-schema conversion.

   Should be rebound thread-locally since there is concurrency control
   is not performed.")

(declaim (inline wire-schema->bytes bytes->wire-schema))

(defun wire-schema->bytes (wire-schema)
  "Convert WIRE-SCHEMA to an ASCII representation stored in an
   octet-vector."
  (declare #.+optimization-fast+unsafe+)

  (ensure-gethash wire-schema *wire-schema->bytes-cache*
                  (keyword->bytes/no-cache wire-schema)))

(defun bytes->wire-schema (bytes)
  "Return a keyword representing the wire-schema encoded in BYTES."
  (declare #.+optimization-fast+unsafe+)

  (ensure-gethash bytes *bytes->wire-schema-cache*
                  (progn (when (emptyp bytes)
                           (error "~@<Empty wire-schema.~:@>"))
                         (bytes->keyword/no-cache bytes))))

;;; Cache utilities

(defun call-with-caches (bindings thunk)
  "Call THUNK with cache variables bound according to BINDINGS."
  (progv (mapcar #'first bindings) (mapcar #'second bindings)
    (funcall thunk)))

(defmacro with-caches (bindings &body body)
  "Execute BODY with cache variables bound according to
   BINDINGS. BINDINGS has either to be a list the elements of which
   are of the form

     (VAR INITIAL-VALUE-EXPR)

   where VAR names the cache variable and INITIAL-VALUE-EXPR is
   evaluated to produce the initial value of VAR or an expression
   which evaluates to a list of described form."
  (if (typep bindings '(or null (cons (cons symbol (cons t null)))))
      `(let (,@bindings) ,@body)
      `(call-with-caches ,bindings  (lambda () ,@body))))
