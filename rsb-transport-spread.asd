;;;; rsb-transport-spread.asd --- System containing the Spread-based transport.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-transport-spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a RSB transport based on the
                Spread group communication system."
  :depends-on  (:nibbles
                :ironclad
                :cl-protobuf

                (:version :network.spread "0.2")

                (:version :cl-rsb         #.(version/string :revision? t))
                (:version :rsb-protocol   #.(version/string :revision? t)))
  :encoding    :utf-8
  :components  ((:module     "spread"
                 :pathname   "src/transport/spread"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "conversion")

                              (:file       "connection")

                              (:file       "assembly-mixin")
                              (:file       "connector")
                              (:file       "in-connector")
                              (:file       "in-push-connector")
                              (:file       "in-pull-connector")
                              (:file       "out-connector"))))

  :in-order-to ((test-op (test-op :rsb-transport-spread-test))))

(defsystem :rsb-transport-spread-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-transport-spread system."
  :depends-on  ((:version :lift                 "1.7.1")

                (:version :cl-rsb               #.(version/string))
                (:version :rsb-transport-spread #.(version/string))

                (:version :cl-rsb-test          #.(version/string)))
  :properties  ((:spread-port  . #.(or (let ((value (uiop:getenv "SPREAD_PORT")))
                                         (when value (read-from-string value)))
                                       5678)))
  :encoding    :utf-8
  :components  ((:module     "spread"
                 :pathname   "test/transport/spread"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "connection")
                              (:file       "connectors")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-transport-spread-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string
         "(network.spread:with-daemon (:port (asdf:component-property
                                              (asdf:find-system :rsb-transport-spread-test)
                                              :spread-port))
            (lift:run-tests :config (lift::lift-relative-pathname
                                     \"lift-transport-spread.config\")))")))
