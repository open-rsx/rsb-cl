;;;; rsb-transport-socket.asd --- System containing the socket transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-transport-socket"
  :description "Socket-based transport for RSB."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "usocket"      "0.6.4") ; for `socket-shutdown'
                "cl-protobuf"

                (:version "rsb"          (:read-file-form "version-string.sexp"))
                (:version "rsb-protocol" (:read-file-form "version-string.sexp")))

  :components  ((:module     "socket"
                 :pathname   "src/transport/socket"
                 :serial     t
                 :components ((:file       "puri-patch")

                              (:file       "package")
                              (:file       "protocol")
                              (:file       "conditions")

                              (:file       "util")
                              (:file       "conversion")

                              (:file       "transport")
                              (:file       "bus-connection")
                              (:file       "bus")
                              (:file       "bus-client")
                              (:file       "bus-server")

                              (:file       "connectors")

                              (:file       "socket-tcp")
                              (:file       "transport-tcp")
                              (:file       "connectors-tcp")

                              (:file       "socket-unix"
                                           :if-feature (:and :sbcl :linux))
                              (:file       "transport-unix"
                                           :if-feature (:and :sbcl :linux))
                              (:file       "connectors-unix"
                                           :if-feature (:and :sbcl :linux)))))

  :in-order-to ((test-op (test-op "rsb-transport-socket/test"))))

(defsystem "rsb-transport-socket/test"
  :description "Unit Tests for the rsb-transport-socket system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "lift"                 "1.7.1")

                (:version "rsb-transport-socket" (:read-file-form "version-string.sexp"))

                (:version "rsb/test"             (:read-file-form "version-string.sexp")))

  :components  ((:module     "socket"
                 :pathname   "test/transport/socket"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "bus")

                              (:file       "transport-tcp")
                              (:file       "connectors-tcp")

                              (:file       "transport-unix"
                                           :if-feature (:and :sbcl :linux))
                              (:file       "connectors-unix"
                                           :if-feature (:and :sbcl :linux)))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(lift:run-tests :config (lift::lift-relative-pathname
                                                  \"lift-transport-socket.config\"))"))))
