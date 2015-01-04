;;;; rsb-xpath-filter.asd --- System containing XPath-based filter.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-filter-xpath
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a filter class that uses XPath
                expressions to discriminate events."
  :depends-on  (:xpath

                (:version :cl-rsb #. (version/string :revision? t)))
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "xpath-filter"))))

  :in-order-to ((test-op (test-op :rsb-filter-xpath-test))))

(defsystem :rsb-filter-xpath-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-transport-spread system."
  :depends-on  ((:version :lift             "1.7.1")

                (:version :cl-rsb           #.(version/string))
                (:version :rsb-filter-xpath #.(version/string))

                (:version :cl-rsb-test      #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "filter"
                 :pathname   "test/filter"
                 :components ((:file       "xpath-filter")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-filter-xpath-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string
         "(lift:run-tests :config (lift::lift-relative-pathname
                                   \"lift-filter-xpath.config\"))")))
