;;;; rsb-clon.asd --- System for interoperation with clon.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-clon"
  :description "Generate clon option descriptions based on
                introspection of RSB configuration options."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "rsb" (:read-file-form "version-string.sexp"))
                "net.didierverna.clon")

  :components  ((:file       "clon"
                 :pathname   "src/clon")))
