;;;; rsb-transport-socket.asd --- System containing the socket transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-transport-socket
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a simple socket-based transport."
  :depends-on  ((:version :usocket      "0.6.4") ; for `socket-shutdown'
                :cl-protobuf

                (:version :cl-rsb       #.(version/string :revision? t))
                (:version :rsb-protocol #.(version/string :revision? t)))
  :encoding    :utf-8
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

                              (:file       "connector")
                              (:file       "in-connector")
                              (:file       "in-pull-connector")
                              (:file       "in-push-connector")
                              (:file       "out-connector"))))

  :in-order-to ((test-op (test-op :rsb-transport-socket-test))))

(defsystem :rsb-transport-socket-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit Tests for the rsb-transport-socket system."
  :depends-on  ((:version :lift                 "1.7.1")

                (:version :rsb-transport-socket #.(version/string))

                (:version :cl-rsb-test          #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "socket"
                 :pathname   "test/transport/socket"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "bus")
                              (:file       "connectors")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-transport-socket-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string
         "(lift:run-tests :config (lift::lift-relative-pathname
                                   \"lift-transport-socket.config\"))")))
