;;;; cl-rsb.asd --- Common Lisp implementation of RSB.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsb-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string)

  (:export
   #:+protocol-directory+)

  (:export
   #:+optimization-fast+unsafe+))

(cl:in-package #:cl-rsb-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 14
  "Minor component of version number.")

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
                     (revision? t)
                     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
   where REVISION and COMMIT are optional.

   REVISION? controls whether REVISION should be included. Default
   behavior is to include REVISION.

   COMMIT? controls whether COMMIT should be included. Default
   behavior is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
          (when revision? (list +version-revision+))
          (when (and commit? +version-commit+)
            (list +version-commit+))))

(defun version/string (&rest args
                       &key
                       revision?
                       commit?)
  "Return a version string of the form
   \"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
   optional.

   See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))

;;; Settings

;; Return a relative pathname which, when MERGE-PATHNAMESed with
;; ANCHOR, yields DIRECTORY.
(defun make-relative (directory anchor)
  (assert (and (pathname-directory directory)
               (not (pathname-name directory))))
  (when (eq :relative (first (pathname-directory directory)))
    (return-from make-relative directory))

  (labels ((from-components (pathname)
             (make-pathname :directory pathname))
           (relative-from-components (components)
             (from-components (list* :relative components))))
    (let ((initial (from-components (pathname-directory anchor))))
      (loop for current = initial then (from-components
                                        (butlast (pathname-directory current)))
            for relative = (parse-namestring
                            (enough-namestring directory current))
            while (equal relative directory)
            collect :up into ups
            finally (return (merge-pathnames
                             (relative-from-components
                              (rest (pathname-directory relative)))
                             (relative-from-components ups)))))))

(defparameter +protocol-directory+
  (make-relative
   (parse-namestring
    (cond
      ((boundp 'cl-user::*rsb.protocol-directory*)
       (symbol-value 'cl-user::*rsb.protocol-directory*))
      ((let* ((protocol-directory-file
                (merge-pathnames "protocol-directory.sexp" *load-truename*))
              (stream (when (probe-file protocol-directory-file)
                        (open protocol-directory-file))))
         (unwind-protect
              (when stream (read stream))
           (when stream (ignore-errors (close stream))))))
      (t "data/")))
   *load-truename*)
  "Directory from which protocol definitions should be loaded.")

(defparameter +optimization-fast+unsafe+
  (if (boundp '+optimization-fast+unsafe+)
      (symbol-value '+optimization-fast+unsafe+)
      '(optimize (speed 3) (compilation-speed 0) (space 0) (debug 0) (safety 0))))

;;; System definitions

(defsystem :cl-rsb
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "A Common Lisp implementation of RSB."
  :depends-on  (:alexandria
                :split-sequence
                :iterate
                (:version :let-plus                      "0.2")
                :more-conditions
                (:version :utilities.print-items         "0.1")
                (:version :utilities.binary-dump         "0.1")

                (:version :bordeaux-threads              "0.8.4")
                (:version :lparallel                     "2.3.2")
                :trivial-garbage
                :closer-mop
                :cl-hooks
                (:version :architecture.service-provider "0.1")
                :log4cl

                :nibbles
                :puri
                :uuid
                :local-time)
  :encoding    :utf-8
  :components  ((:module     "src-early"
                 :pathname   "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "scope")
                              (:file       "uris")
                              (:file       "variables")
                              (:file       "util")

                              (:file       "mixins")
                              (:file       "event")

                              (:file       "error-handling")))

                (:module     "event-processing-early"
                 :pathname   "src/event-processing"
                 :depends-on ("src-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "protocol")

                              (:file       "scope-trie")))

                (:module     "filter"
                 :pathname   "src/filter"
                 :depends-on ("src-early" "event-processing-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "filter-mixins")
                              (:file       "composite-filter")

                              (:file       "scope-filter")
                              (:file       "type-filter")
                              (:file       "origin-filter")
                              (:file       "method-filter")
                              (:file       "meta-data-filter")
                              (:file       "cause-filter")))

                (:module     "transform"
                 :pathname   "src/transform"
                 :depends-on ("src-early" "event-processing-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "mixins")

                              (:file       "prefix-scope")
                              (:file       "drop-payload")
                              (:file       "adjust-timestamps")))

                (:module     "event-processing"
                 :pathname   "src/event-processing"
                 :depends-on ("src-early" "event-processing-early"
                              "filter" "transform") ; for {filter,transform}-mixin
                 :serial     t
                 :components ((:file       "broadcast-processor")
                              (:file       "pull-processor")

                              (:file       "processor-mixins")

                              (:file       "configurator")
                              (:file       "in-route-configurator")
                              (:file       "out-route-configurator")
                              (:file       "client")))

                (:module     "converter"
                 :pathname   "src/converter"
                 :depends-on ("src-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "macros")

                              (:file       "sequence")
                              (:file       "force-wire-schema")
                              (:file       "annotating")

                              (:file       "fundamental")

                              (:file       "reader")))

                (:module     "transport"
                 :pathname   "src/transport"
                 :depends-on ("src-early"
                              "event-processing" ; for error-policy-mixin
                              "converter")       ; for conversion-mixin
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "transport")
                              (:file       "connector-class")
                              (:file       "connector")

                              (:file       "connector-mixins")))

                (:module      "src"
                 :depends-on  ("src-early"
                               "event-processing"
                               "transport")
                 :serial     t
                 :components ((:file       "configuration")

                              (:file       "participant")
                              (:file       "receiving-client")
                              (:file       "listener")
                              (:file       "reader")
                              (:file       "informer")

                              (:file       "macros")

                              (:file       "reloading")))

                (:module     "patterns"
                 :pathname   "src/patterns"
                 :depends-on ("src")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "mixins")))

                (:module     "patterns-request-reply"
                 :pathname   "src/patterns/request-reply"
                 :depends-on ("src" "patterns")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "future")

                              (:file       "server")
                              (:file       "local-server")
                              (:file       "remote-server")

                              (:file       "macros"))))

  :in-order-to ((test-op (test-op :cl-rsb-test))))

(defsystem :cl-rsb-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the cl-rsb system."
  :depends-on  ((:version :lift                    "1.7.1")

                (:version :cl-rsb                  #.(version/string))
                (:version :rsb-transport-inprocess #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "random")

                              (:file       "protocol")
                              (:file       "scope")
                              (:file       "event")
                              (:file       "uris")
                              (:file       "util")
                              (:file       "configuration")
                              (:file       "mixins")
                              (:file       "participant")
                              (:file       "listener")
                              (:file       "reader")
                              (:file       "informer")
                              (:file       "macros")))

                (:file       "examples"
                 :pathname   "test/examples"
                 :depends-on ("test"))

                (:module     "filter"
                 :pathname   "test/filter"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "composite-filter")

                              (:file       "scope-filter")
                              (:file       "type-filter")
                              (:file       "origin-filter")
                              (:file       "method-filter")
                              (:file       "meta-data-filter")
                              (:file       "cause-filter")))

                (:module     "transform"
                 :pathname   "test/transform"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "prefix-scope")
                              (:file       "drop-payload")
                              (:file       "adjust-timestamps")))

                (:module     "event-processing"
                 :pathname   "test/event-processing"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")

                              (:file       "scope-trie")

                              (:file       "processor-mixins")
                              (:file       "in-route-configurator")))

                (:module     "converter"
                 :pathname   "test/converter"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "annotating")
                              (:file       "fundamental")
                              (:file       "reader")))

                (:module     "transport"
                 :pathname   "test/transport"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connector-class")
                              (:file       "threaded-receiver-mixin")
                              (:file       "error-handling-mixins")
                              (:file       "restart-mixins")
                              (:file       "connector")))

                (:module     "patterns"
                 :pathname   "test/patterns"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "mixins")))

                (:module     "patterns-request-reply"
                 :pathname   "test/patterns/request-reply"
                 :depends-on ("test" "patterns")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "future")
                              (:file       "server")
                              (:file       "local-server")
                              (:file       "remote-server")
                              (:file       "macros")

                              (:file       "integration")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rsb-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string "(lift:run-tests :config :generic)")))
