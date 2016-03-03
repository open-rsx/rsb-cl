;;;; rsb-filter-regex.asd --- System containing regex-based filter.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-filter-regex"
  :description "Regular expression filter for events with text payloads."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ("cl-ppcre"

                (:version "rsb" (:read-file-form "version-string.sexp")))

  :components  ((:module     "filter"
                 :pathname   "src/filter"
                 :components ((:file       "regex-filter"))))

  :in-order-to ((test-op (test-op "rsb-filter-regex/test"))))

(defsystem "rsb-filter-regex/test"
  :description "Unit tests for the rsb-filter-regex system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "lift"             "1.7.1")

                (:version "rsb"              (:read-file-form "version-string.sexp"))
                (:version "rsb-filter-regex" (:read-file-form "version-string.sexp"))

                (:version "rsb/test"         (:read-file-form "version-string.sexp")))

  :components  ((:module     "filter"
                 :pathname   "test/filter"
                 :components ((:file       "regex-filter"))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(lift:run-tests :config (lift::lift-relative-pathname
                                                  \"lift-filter-regex.config\"))"))))
