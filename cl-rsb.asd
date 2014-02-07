;;;; cl-rsb.asd --- Common Lisp implementation of RSB.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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
   #:+optimization-fast+unsafe+))

(cl:in-package #:cl-rsb-system)

#+sbcl (asdf:load-system :sb-posix)

(when (find-system :asdf-system-connections nil)
  (let ((*compile-print*   nil)
        (*compile-verbose* nil))
    (load-system :asdf-system-connections)))

(when (find-symbol "DOC-OP" :asdf)
  (pushnew :asdf-doc-op *features*))

#-asdf-doc-op
(progn
  (defclass doc-op (operation)
    ()
    (:documentation
     "Poor man's doc-op until ASDF gets one."))

  (defmethod perform ((op doc-op) (component t))
    nil))

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 11
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

COMMIT? controls whether COMMIT should be included. Default behavior
is to not include COMMIT."
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
  :license     "LGPLv3; see COPYING file for details."
  :description "A Common Lisp implementation of RSB."
  :depends-on  (:alexandria
                :split-sequence
                :iterate
                (:version :let-plus  "0.2")
                :more-conditions

                :bordeaux-threads
                (:version :lparallel "2.3.2")
                :trivial-garbage
                :closer-mop
                :cl-hooks
                :cl-dynamic-classes
                :log4cl

                :nibbles
                :puri
                :uuid
                :local-time)
  :encoding    :utf-8
  :components  ((:module     "src-early"
                 :pathname   "src"
                 :components ((:file       "package")
                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "variables"
                               :depends-on ("package"))
                              (:file       "util"
                               :depends-on ("package" "types" "protocol"
                                            "variables"))

                              (:file       "scope"
                               :depends-on ("package" "types" "protocol"))
                              (:file       "uris"
                               :depends-on ("package" "scope"))

                              (:file       "mixins"
                               :depends-on ("package" "types" "protocol"
                                            "scope"))
                              (:file       "event"
                               :depends-on ("package" "types" "util"
                                            "mixins"))

                              (:file       "error-handling"
                               :depends-on ("package"))))

                (:module     "filter"
                 :pathname   "src/filter"
                 :depends-on ("src-early")
                 :components ((:file       "package")
                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))

                              (:file       "filter-mixins"
                               :depends-on ("package" "protocol"))
                              (:file       "composite-filter"
                               :depends-on ("package" "protocol"
                                            "filter-mixins"))

                              (:file       "scope-filter"
                               :depends-on ("package" "protocol"
                                            "filter-mixins"))
                              (:file       "type-filter"
                               :depends-on ("package" "protocol"
                                            "filter-mixins"))
                              (:file       "origin-filter"
                               :depends-on ("package" "protocol"
                                            "filter-mixins"))
                              (:file       "method-filter"
                               :depends-on ("package" "protocol"
                                            "filter-mixins"))))

                (:module     "event-processing"
                 :pathname   "src/event-processing"
                 :depends-on ("src-early" "filter") ; for filtering-processor-mixin
                 :components ((:file       "package")
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package" "conditions"))

                              (:file       "broadcast-processor"
                               :depends-on ("package" "protocol"))
                              (:file       "pull-processor"
                               :depends-on ("package" "protocol"))

                              (:file       "processor-mixins"
                               :depends-on ("package" "protocol"))

                              (:file       "configurator"
                               :depends-on ("package"
                                            "broadcast-processor"
                                            "pull-processor"
                                            "processor-mixins"))
                              (:file       "in-route-configurator"
                               :depends-on ("package" "util" "configurator"))
                              (:file       "out-route-configurator"
                               :depends-on ("package" "configurator"))
                              (:file       "client"
                               :depends-on ("package" "configurator"))))

                (:module     "converter"
                 :pathname   "src/converter"
                 :depends-on ("src-early")
                 :components ((:file       "package")
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "macros"
                               :depends-on ("package" "protocol"))

                              (:file       "sequence"
                               :depends-on ("package" "conditions"
                                            "protocol"))
                              (:file       "force-wire-schema"
                               :depends-on ("package" "protocol"))

                              (:file       "fundamental"
                               :depends-on ("package" "conditions"
                                            "protocol" "macros"))

                              (:file       "reader"
                               :depends-on ("package" "conditions"
                                            "protocol"))))

                (:module     "transport"
                 :pathname   "src/transport"
                 :depends-on ("src-early"
                              "event-processing" ; for error-policy-mixin
                              "converter")       ; for conversion-mixin
                 :components ((:file       "package")
                              (:file       "variables"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package" "conditions"))
                              (:file       "connector-class"
                               :depends-on ("package" "protocol"))
                              (:file       "connector"
                               :depends-on ("package" "protocol"
                                            "connector-class"))

                              (:file       "connector-mixins"
                               :depends-on ("package" "protocol" "connector-class"))))

                (:module      "src"
                 :depends-on  ("src-early"
                               "event-processing"
                               "transport")
                 :components ((:file       "configuration")

                              (:file       "participant"
                               :depends-on ("configuration"))
                              (:file       "receiving-client")
                              (:file       "listener"
                               :depends-on ("participant"
                                            "receiving-client"))
                              (:file       "reader"
                               :depends-on ("participant"
                                            "receiving-client"))
                              (:file       "informer"
                               :depends-on ("participant"))

                              (:file       "macros"
                               :depends-on ("listener" "reader"
                                             "informer"))

                              (:file       "reloading")))

                (:module     "transport-inprocess"
                 :pathname   "src/transport/inprocess"
                 :depends-on ("transport" "filter")
                 :components ((:file       "package")
                              (:file       "connector"
                               :depends-on ("package"))
                              (:file       "in-push-connector"
                               :depends-on ("package" "connector"))
                              (:file       "in-pull-connector"
                               :depends-on ("package" "connector"))
                              (:file       "out-connector"
                               :depends-on ("package" "connector"))))

                (:module     "patterns"
                 :pathname   "src/patterns"
                 :depends-on ("src")
                 :components ((:file       "package")))

                (:module     "patterns-request-reply"
                 :pathname   "src/patterns/request-reply"
                 :depends-on ("src" "patterns")
                 :components ((:file       "package")
                              (:file       "types"
                               :depends-on ("package"))
                              (:file       "variables"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package" "types"))

                              (:file       "future"
                               :depends-on ("package" "protocol"))

                              (:file       "server"
                               :depends-on ("package" "protocol"))
                              (:file       "local-server"
                               :depends-on ("package" "variables"
                                            "protocol" "server"))
                              (:file       "remote-server"
                               :depends-on ("package" "variables"
                                            "protocol" "server"))

                              (:file       "macros"
                               :depends-on ("package" "local-server"
                                            "remote-server")))))

  :in-order-to ((doc-op  (doc-op  :cl-rsb-doc))
                (test-op (test-op :cl-rsb-test))))

(defsystem :cl-rsb-doc
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "Documentation generation for the cl-rsb system."
  :depends-on  (:cxml-stp
                :cl-protobuf
                :network.spread
                :cl-rsb)
  :encoding    :utf-8
  :components  ((:module     "doc"
                 :components ((:file       "package"))))

  :in-order-to ((doc-op (load-op :cl-rsb-doc))))

(defmethod perform ((op doc-op) (system (eql (find-system :cl-rsb-doc))))
  (eval (read-from-string "(RSB.DOC:DOCUMENT-SYSTEM)")))

(defsystem :cl-rsb-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "Unit Tests for the cl-rsb system."
  :depends-on  ((:version :lift   "1.7.1")

                :cl-protobuf
                :network.spread
                :usocket

                (:version :cl-rsb #.(version/string)))
  :properties  ((:spread-port  . #.(or #+sbcl (let ((value (sb-posix:getenv "SPREAD_PORT")))
                                                (when value (read-from-string value)))
                                       5678)))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :components ((:file       "package")

                              (:file       "scope"
                               :depends-on ("package"))
                              (:file       "event"
                               :depends-on ("package"))
                              (:file       "uris"
                               :depends-on ("package"))
                              (:file       "mixins"
                               :depends-on ("package"))
                              (:file       "participant"
                               :depends-on ("package"))
                              (:file       "listener"
                               :depends-on ("package"))
                              (:file       "reader"
                               :depends-on ("package"))
                              (:file       "informer"
                               :depends-on ("package"))

                              (:file       "macros"
                               :depends-on ("package"))))

                (:module     "filter"
                 :pathname   "test/filter"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "disjoin-filter"
                               :depends-on ("package"))
                              (:file       "conjoin-filter"
                               :depends-on ("package"))
                              (:file       "scope-filter"
                               :depends-on ("package"))
                              (:file       "method-filter"
                               :depends-on ("package"))))

                (:module     "event-processing"
                 :pathname   "test/event-processing"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "error-policy-mixin"
                               :depends-on ("package"))
                              (:file       "error-handling-dispatcher-mixin"
                               :depends-on ("package"))
                              (:file       "processor-mixins"
                               :depends-on ("package"))

                              (:file       "in-route-configurator"
                               :depends-on ("package"))))

                (:module     "converter"
                 :pathname   "test/converter"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "fundamental"
                               :depends-on ("package"))
                              (:file       "reader"
                               :depends-on ("package"))))

                (:module     "transport"
                 :pathname   "test/transport"
                 :depends-on ("test")
                 :components ((:file       "package")
                              (:file       "connector-class"
                               :depends-on ("package"))
                              (:file       "threaded-receiver-mixin"
                               :depends-on ("package"))
                              (:file       "error-handling-mixins"
                               :depends-on ("package"))
                              (:file       "restart-mixins"
                               :depends-on ("package"))))

                (:module     "inprocess"
                 :pathname   "test/transport/inprocess"
                 :depends-on ("test" "transport")
                 :components ((:file       "package")
                              (:file       "in-pull-connector"
                               :depends-on ("package"))
                              (:file       "in-push-connector"
                               :depends-on ("package"))
                              (:file       "out-connector"
                               :depends-on ("package"))))

                (:module     "socket"
                 :pathname   "test/transport/socket"
                 :depends-on ("test" "transport")
                 :components ((:file       "package")
                              (:file       "bus"
                               :depends-on ("package"))
                              (:file       "connectors"
                               :depends-on ("package"))))

                (:module     "spread"
                 :pathname   "test/transport/spread"
                 :depends-on ("test" "transport")
                 :components ((:file       "package")
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "fragmentation"
                               :depends-on ("package"))
                              (:file       "connection"
                               :depends-on ("package"))
                              (:file       "connectors"
                               :depends-on ("package"))))

                (:module     "patterns"
                 :pathname   "test/patterns"
                 :depends-on ("test")
                 :components ((:file       "package")))

                (:module     "patterns-request-reply"
                 :pathname   "test/patterns/request-reply"
                 :depends-on ("test" "patterns")
                 :components ((:file       "package")
                              (:file       "future"
                               :depends-on ("package"))
                              (:file       "server"
                               :depends-on ("package"))
                              (:file       "local-server"
                               :depends-on ("package"))
                              (:file       "macros"
                               :depends-on ("package"))

                              (:file       "integration"
                               :depends-on ("package")))))

  :in-order-to ((test-op (load-op :cl-rsb-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rsb-test))))
  (eval (read-from-string
         "(NETWORK.SPREAD:WITH-DAEMON (:PORT (ASDF:COMPONENT-PROPERTY
                                               (ASDF:FIND-SYSTEM :CL-RSB-TEST) :SPREAD-PORT))
            (LIFT:RUN-TESTS :CONFIG :GENERIC))")))

;;; System connection with clon

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-com.dvlsoft.clon
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "Generate clon option descriptions based on
introspection of RSB configuration options."
  :requires    (cl-rsb
                com.dvlsoft.clon)
  :encoding    :utf-8
  :components  ((:file       "clon"
                 :pathname   "src/clon")))

;;; System connection with network.spread

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-network.spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system connections provides a RSB transport based
on the spread group communication system."
  :requires    (cl-rsb
                network.spread
                cl-protobuf
                cl-rsb-and-cl-protobuf)
  :depends-on  (:nibbles
                :ironclad)
  :encoding    :utf-8
  :components  ((:module     "spread"
                 :pathname   "src/transport/spread"
                 :components ((:file       "package")
                              (:file       "conditions"
                               :depends-on ("package"))
                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "fragmentation"
                               :depends-on ("package" "conditions" "util"))
                              (:file       "conversion"
                               :depends-on ("package" "conditions" "fragmentation"))

                              (:file       "connection"
                               :depends-on ("package" "util"))

                              (:file       "assembly-mixin"
                               :depends-on ("package" "fragmentation"))
                              (:file       "connector"
                               :depends-on ("package" "util"
                                            "connection" "conversion"))
                              (:file       "in-connector"
                               :depends-on ("package" "connector"
                                            "assembly-mixin"))
                              (:file       "in-push-connector"
                               :depends-on ("package" "in-connector"))
                              (:file       "in-pull-connector"
                               :depends-on ("package" "in-connector"))
                              (:file       "out-connector"
                               :depends-on ("package" "connector"))))))

;;; System connection with cl-protobuf

(or (when (find-system :cl-protobuf nil)
      (let ((*compile-print*   nil)
            (*compile-verbose* nil))
        (asdf:load-system :cl-protobuf)))
    (warn "~@<Cannot load definition of system connection ~
cl-rsb-and-cl-protobuf unless system cl-protobuf can be loaded~@:>"))

#+(and asdf-system-connections asdf-protocol-buffer-descriptors)
(defsystem-connection :cl-rsb-and-cl-protobuf
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system connection provides methods to serialize/deserialize
RSB events to/from Google protocol buffers."
  :requires    (cl-rsb
                cl-protobuf)
  :encoding    :utf-8
  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   #.+protocol-directory+
                 :components ((:file       "EventId"
                               :pathname   "rsb/protocol/EventId")
                              (:file       "EventMetaData"
                               :pathname   "rsb/protocol/EventMetaData"
                               :depends-on ("EventId"))
                              (:file       "Notification"
                               :pathname   "rsb/protocol/Notification"
                               :depends-on ("EventId" "EventMetaData"))
                              (:file       "FragmentedNotification"
                               :pathname   "rsb/protocol/FragmentedNotification"
                               :depends-on ("EventId" "EventMetaData" "Notification"))

                              (:file       "EventsByScopeMap"
                               :pathname   "rsb/protocol/collections/EventsByScopeMap"
                               :depends-on ("Notification"))))

                (:module     "converter-protocol-buffer"
                 :pathname   "src/converter"
                 :components ((:file       "protocol-buffers")))))

;;; System connection with usocket

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-usocket
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system connection provides a simple socket-based connector."
  :requires    (cl-rsb
                cl-rsb-and-cl-protobuf
                usocket)
  :encoding    :utf-8
  :components  ((:module     "socket"
                 :pathname   "src/transport/socket"
                 :components ((:file       "package")
                              (:file       "protocol"
                               :depends-on ("package"))
                              (:file       "conditions"
                               :depends-on ("package"))

                              (:file       "util"
                               :depends-on ("package"))
                              (:file       "conversion"
                               :depends-on ("package"))

                              (:file       "bus-connection"
                               :depends-on ("package" "util" "conversion"))
                              (:file       "bus"
                               :depends-on ("package" "bus-connection"))
                              (:file       "bus-client"
                               :depends-on ("package" "bus"))
                              (:file       "bus-server"
                               :depends-on ("package" "util" "bus"))

                              (:file       "connector"
                               :depends-on ("package"
                                            "bus-client" "bus-server"
                                            "conversion"))
                              (:file       "in-connector"
                               :depends-on ("package" "connector"))
                              (:file       "in-pull-connector"
                               :depends-on ("package" "in-connector"))
                              (:file       "in-push-connector"
                               :depends-on ("package" "in-connector"))
                              (:file       "out-connector"
                               :depends-on ("package" "connector"))))))

;;; System connection with cl-ppcre

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-cl-ppcre
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system connection provides a regular expression
filter for events with text content."
  :requires    (cl-rsb
                cl-ppcre)
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "regex-filter")))))

;;; System connection with xpath

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-xpath
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system connection provides a filter classs that
uses XPath expressions to discriminate events."
  :requires    (cl-rsb
                xpath)
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "xpath-filter")))))
