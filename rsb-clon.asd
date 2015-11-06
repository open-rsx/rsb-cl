;;;; rsb-clon.asd --- System for interoperation with clon.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-clon
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Generate clon option descriptions based on
                introspection of RSB configuration options."
  :depends-on  ((:version :cl-rsb #.(version/string :revision? t))
                :net.didierverna.clon)
  :encoding    :utf-8
  :components  ((:file       "clon"
                 :pathname   "src/clon")))
