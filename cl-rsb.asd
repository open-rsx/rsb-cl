;;; cl-rsb.asd --- Common Lisp implementation of RSB.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :cl-rsb-system
  (:use
   :cl
   :asdf)

  (:export
   :version
   :version/string))

(cl:in-package :cl-rsb-system)

#+sbcl (asdf:load-system :sb-posix)

(when (find-system :asdf-system-connections nil)
  (load-system :asdf-system-connections))

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
;;

(defconstant +version-major+ 0
  "Major version number.")

(defconstant +version-minor+ 3
  "Major version number.")

(defconstant +version-revision+ 0
  "Major version number.")

(defun version ()
  "Return a version of the form (MAJOR MINOR REVISION) "
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version)))


;;; System definitions
;;

(defsystem :cl-rsb
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "A Common Lisp implementation of RSB."
  :depends-on  (:alexandria
		:split-sequence
		:iterate
		:metabang-bind

		#+sbcl sb-concurrency
		:bordeaux-threads ;; or eager-future
		:trivial-garbage
		:closer-mop
		:cl-hooks
		:cl-dynamic-classes
		:log5

		:puri
		:uuid
		:local-time)
  :components  ((:module     "src-early"
		 :pathname   "src"
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "scope"
			       :depends-on ("package" "protocol"))
			      (:file       "event"
			       :depends-on ("package" "util"))
			      (:file       "uris"
			       :depends-on ("package" "scope"))

			      (:file       "util"
			       :depends-on ("package" "scope"
					    "protocol"))))

		(:module     "filter"
		 :pathname   "src/filter"
		 :depends-on ("src-early")
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "filter-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "composite-filter"
			       :depends-on ("package" "protocol"
					    "filter-mixin"))

			      (:file       "scope-filter"
			       :depends-on ("package" "protocol"
					    "filter-mixin"))
			      (:file       "type-filter"
			       :depends-on ("package" "protocol"
					    "filter-mixin"))))

		(:module     "event-processing"
		 :pathname   "src/event-processing"
		 :depends-on ("src-early" "filter") ;; for filtering-processor-mixin
		 :components ((:file       "package")
			      (:file       "util"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "broadcast-processor"
			       :depends-on ("package" "protocol"))
			      (:file       "pull-processor"
			       :depends-on ("package" "protocol"))
			      (:file       "filtering-processor-mixin"
			       :depends-on ("package" "protocol"))

			      (:file       "configurator"
			       :depends-on ("package"
					    "broadcast-processor"
					    "pull-processor"))
			      (:file       "in-route-configurator"
			       :depends-on ("package" "util" "configurator"))
			      (:file       "out-route-configurator"
			       :depends-on ("package" "configurator"))
			      (:file       "client"
			       :depends-on ("package" "configurator"))))

		(:module      "src"
		 :depends-on  ("src-early"
			       "event-processing")
		 :components ((:file       "configuration")
			      (:file       "variables"
			       :depends-on ("configuration"))

			      (:file       "participant")
			      (:file       "listener"
			       :depends-on ("participant"))
			      (:file       "reader"
			       :depends-on ("participant"))
			      (:file       "informer"
			       :depends-on ("participant"))

			      (:file       "macros"
			       :depends-on ("listener" "reader"
					    "informer"))))

		(:module     "converter"
		 :pathname   "src/converter"
		 :depends-on ("src")
		 :components ((:file       "package")
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "protocol"
			       :depends-on ("package"))

			      (:file       "sequence"
			       :depends-on ("package" "conditions"
					    "protocol"))
			      (:file       "fundamental"
			       :depends-on ("package" "conditions"
					    "protocol"))

			      (:file       "reader"
			       :depends-on ("package" "conditions"
					    "protocol"))))

		(:module     "transport"
		 :pathname   "src/transport"
		 :depends-on ("src" "converter") ;; for conversion-mixin
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "connector"
			       :depends-on ("package" "protocol"))

			      (:file       "conversion-mixin"
			       :depends-on ("package"))
			      (:file       "message-receiver-mixin"
			       :depends-on ("package"))))

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
			       :depends-on ("package" "connector")))))
  :in-order-to ((doc-op  (doc-op  :cl-rsb-doc))
		(test-op (test-op :cl-rsb-test))))

(defsystem :cl-rsb-doc
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "Documentation generation for the cl-rsb system."
  :depends-on  (:cxml-stp
		:cl-protobuf
		:cl-spread
		:cl-rsb)
  :components  ((:module     "doc"
		 :components ((:file       "package"))))
  :in-order-to ((doc-op (load-op :cl-rsb-doc))))

(defmethod perform ((op doc-op) (system (eql (find-system :cl-rsb-doc))))
  (eval (read-from-string "(RSB.DOC:DOCUMENT-SYSTEM)")))

(defsystem :cl-rsb-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.3.0"
  :license     "GPL3; see COPYING file for details."
  :description "Unit Tests for the cl-rsb system."
  :depends-on  (:lift
		:cxml-stp
		:cl-protobuf
		:cl-spread
		:cl-rsb)
  :properties  ((:spread-port  . #.(or #+sbcl (let ((value (sb-posix:getenv "SPREAD_PORT")))
						(when value (read-from-string value)))
				       5678)))
  :components  ((:module     "test"
		 :components ((:file       "package")

			      (:file       "scope"
			       :depends-on ("package"))
			      (:file       "event"
			       :depends-on ("package"))
			      (:file       "uris"
			       :depends-on ("package"))
			      (:file       "listener"
			       :depends-on ("package"))
			      (:file       "informer"
			       :depends-on ("package"))))

		(:module     "filter"
		 :pathname   "test/filter"
		 :depends-on ("test")
		 :components ((:file       "package")
			      (:file       "disjoin-filter"
			       :depends-on ("package"))
			      (:file       "conjoin-filter"
			       :depends-on ("package"))
			      (:file       "scope-filter"
			       :depends-on ("package"))))

		(:module     "converter"
		 :pathname   "test/converter"
		 :depends-on ("test")
		 :components ((:file       "package")
			      (:file       "fundamental"
			       :depends-on ("package"))))

		(:module     "transport"
		 :pathname   "test/transport"
		 :depends-on ("test")
		 :components ((:file       "package")))

		(:module     "spread"
		 :pathname   "test/transport/spread"
		 :depends-on ("test" "transport")
		 :components ((:file       "package")
			      (:file       "util"
			       :depends-on ("package"))
			      (:file       "fragmentation"
			       :depends-on ("package")))))
  :in-order-to ((test-op (load-op :cl-rsb-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rsb-test))))
  (eval (read-from-string
	 "(SPREAD:WITH-DAEMON (:PORT (ASDF:COMPONENT-PROPERTY
                                       (ASDF:FIND-SYSTEM :CL-RSB-TEST) :SPREAD-PORT))
            (LIFT:RUN-TESTS :CONFIG :GENERIC))")))


;;; System connection with clon
;;

(defsystem-connection :cl-rsb-and-com.dvlsoft.clon
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "Generate clon option descriptions based on
introspection of RSB configuration options."
  :requires    (cl-rsb
		com.dvlsoft.clon)
  :components  ((:file       "clon"
		 :pathname   "src/clon")))


;;; System connection with cl-spread
;;

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-cl-spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.3.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system connections provides a RSB transport based
on the spread group communication system."
  :requires    (cl-rsb
		cl-spread
		cl-protobuf
		cl-rsb-and-cl-protobuf)
  :depends-on  (:ironclad)
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
			       :depends-on ("package" "fragmentation"))

			      (:file       "connection"
			       :depends-on ("package" "util"))

			      (:file       "assembly-mixin"
			       :depends-on ("package" "fragmentation"))
			      (:file       "connector"
			       :depends-on ("package" "util"
					    "connection" "conversion"))
			      (:file       "in-connector"
			       :depends-on ("package" "connector"))
			      (:file       "in-push-connector"
			       :depends-on ("package" "in-connector"))
			      (:file       "in-pull-connector"
			       :depends-on ("package" "in-connector"))
			      (:file       "out-connector"
			       :depends-on ("package" "connector"))))))


;;; System connection with cl-protobuf
;;

(or (when (find-system :cl-protobuf nil)
      (load-system :cl-protobuf))
    (warn "~@<Cannot load definition of system connection ~
cl-rsb-and-cl-protobuf unless system cl-protobuf can be loaded~@:>"))

#+(and asdf-system-connections asdf-protocol-buffer-descriptors)
(defsystem-connection :cl-rsb-and-cl-protobuf
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.3.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system connections provides methods to serialize/deserialize
RSB events to/from Google protocol buffers."
  :requires    (cl-rsb
		cl-protobuf)
  :components  ((:module     "protocol"
		 :pathname   "data"
		 :default-component-class asdf::protocol-buffer-descriptor
		 :components ((:file       "Protocol")))

		(:module     "converter-protocol-buffer"
		 :pathname   "src/converter"
		 :components ((:file       "protocol-buffers")))))


;;; System connection with cxml-stp and xpath
;;

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-cxml-stp-and-xpath
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.3.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system connections provides methods to filter RSB
events with XML data."
  :requires    (cl-rsb
		cxml-stp
		xpath)
  :components  ((:module     "filter"
		 :pathname   "src/filter"
		 :components ((:file       "xpath-filter")))))


;;; System connection with cxml-location
;;

#+asdf-system-connections
(defsystem-connection :cl-rsb-and-cxml-location
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.3.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system connections provides methods to serialize/deserialize
RSB events to/from XML documents."
  :requires    (cl-rsb
		cxml-location)
  :components  ((:module     "converter"
		 :pathname   "src/converter"
		 :components ((:file       "xml")))))
