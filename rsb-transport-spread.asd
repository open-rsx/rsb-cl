;;;; rsb-transport-spread.asd --- System containing the Spread-based transport.
;;;;
;;;; Copyright (C) 2011-2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-transport-spread"
  :description "RSB transport based on the Spread group communication system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ("nibbles"
                "ironclad"
                "cl-protobuf"

                (:version "network.spread" "0.3")

                (:version "rsb"            (:read-file-form "version-string.sexp"))
                (:version "rsb-protocol"   (:read-file-form "version-string.sexp")))

  :components  ((:module     "spread"
                 :pathname   "src/transport/spread"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "conversion")

                              (:file       "transport")
                              (:file       "connection")

                              (:file       "assembly-mixin")
                              (:file       "connector")
                              (:file       "in-connector")
                              (:file       "in-push-connector")
                              (:file       "in-pull-connector")
                              (:file       "out-connector"))))

  :in-order-to ((test-op (test-op "rsb-transport-spread/test"))))

(defsystem "rsb-transport-spread/test"
  :description "Unit tests for the rsb-transport-spread system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "lift"                 "1.7.1")

                (:version "rsb"                  (:read-file-form "version-string.sexp"))
                (:version "rsb-transport-spread" (:read-file-form "version-string.sexp"))

                (:version "rsb/test"             (:read-file-form "version-string.sexp")))

  :properties  ((:spread-port  . #.(or (let ((value (uiop:getenv "SPREAD_PORT")))
                                         (when value (read-from-string value)))
                                       5678)))

  :components  ((:module     "spread"
                 :pathname   "test/transport/spread"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "connection")
                              (:file       "connectors"))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(network.spread.daemon:with-daemon
                             (:port (asdf:component-property
                                     (asdf:find-system :rsb-transport-spread/test)
                                     :spread-port))
                           (lift:run-tests :config (lift::lift-relative-pathname
                                                    \"lift-transport-spread.config\")))"))))
