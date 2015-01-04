;;;; rsb-filter-regex.asd --- System containing regex-based filter.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-filter-regex
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a regular expression filter for
                events with text payloads."
  :depends-on  (:cl-ppcre

                (:version :cl-rsb #.(version/string :revision? t)))
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "regex-filter"))))

  :in-order-to ((test-op (test-op :rsb-filter-regex-test))))

(defsystem :rsb-filter-regex-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-filter-regex system."
  :depends-on  ((:version :lift             "1.7.1")

                (:version :cl-rsb           #.(version/string))
                (:version :rsb-filter-regex #.(version/string))

                (:version :cl-rsb-test      #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "test/filter"
                 :components ((:file       "regex-filter")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-filter-regex-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string
         "(lift:run-tests :config (lift::lift-relative-pathname
                                   \"lift-filter-regex.config\"))")))
