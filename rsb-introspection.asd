;;;; rsb-introspection.asd --- System definition for introspection for RSB.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-introspection-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsb-introspection-system)

#.(progn
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(defsystem :rsb-introspection
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsb-system:version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Introspection support for RSB."
  :depends-on  (:utilities.print-items

                (:version :cl-rsb #.(cl-rsb-system:version/string)))
  :encoding    :utf-8
  :components  ((:module     "introspection"
                 :pathname   "src/introspection"
                 :depends-on ("protocol")
                 :serial     t
                 :components ((:file       "package")

                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "variables"))))

  :in-order-to ((test-op (test-op :rsb-introspection-test))))

(defsystem :rsb-introspection-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsb-system:version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-introspection system."
  :depends-on  ((:version :lift              "1.7.1")

                (:version :rsb-introspection #.(cl-rsb-system:version/string))

                (:version :cl-rsb-test       #.(cl-rsb-system:version/string)))
  :encoding    :utf-8
  :components  ((:module     "introspection"
                 :pathname   "test/introspection"
                 :serial     t
                 :components ((:file       "package"))))

  :in-order-to ((test-op (load-op :rsb-introspection-test))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-introspection-test))))
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-introspection-test \"lift-introspection.config\"))")))
