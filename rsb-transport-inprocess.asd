;;;; rsb-transport-inprocess.asd --- System containing the inprocess transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-transport-inprocess"
  :description "Simple and efficient in-process transport."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "rsb" (:read-file-form "version-string.sexp")))

  :components  ((:module     "inprocess"
                 :pathname   "src/transport/inprocess"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "transport")
                              (:file       "connectors"))))

  :in-order-to ((test-op (test-op "rsb-transport-inprocess/test"))))

(defsystem "rsb-transport-inprocess/test"
  :description "Unit Tests for the rsb-transport-inprocess system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "lift"                    "1.7.1")

                (:version "rsb-transport-inprocess" (:read-file-form "version-string.sexp"))

                (:version "rsb/test"                (:read-file-form "version-string.sexp")))

  :components  ((:module     "inprocess"
                 :pathname   "test/transport/inprocess"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connectors"))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(lift:run-tests :config (lift::lift-relative-pathname
                                                  \"lift-transport-inprocess.config\"))"))))
