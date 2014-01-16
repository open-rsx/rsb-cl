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
                 :components ((:file       "xpath-filter")))))
