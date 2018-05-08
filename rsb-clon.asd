;;;; rsb-clon.asd --- System for interoperation with clon.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem "rsb-clon"
  :description "Generate clon option descriptions based on
                introspection of RSB configuration options."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "cl-rsb" #.(version/string :revision? t))
                "net.didierverna.clon")

  :components  ((:file       "clon"
                 :pathname   "src/clon")))
