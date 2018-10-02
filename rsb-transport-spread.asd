;;;; rsb-transport-spread.asd --- System containing the Spread-based transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defparameter *spread-port*
  (or (let ((value (uiop:getenv "SPREAD_PORT")))
        (when value (read-from-string value)))
      5678))

(defsystem "rsb-transport-spread"
  :description "RSB transport based on the Spread group communication system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("nibbles"
                "ironclad"
                "cl-protobuf"

                (:version "network.spread" "0.3")

                (:version "cl-rsb"         #.(version/string :revision? t))
                (:version "rsb-protocol"   #.(version/string :revision? t)))

  :components  ((:module     "spread"
                 :pathname   "src/transport/spread"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "notifications")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "conversion")

                              (:file       "transport")
                              (:file       "connection")

                              (:file       "assembly-mixin")

                              (:file       "sender-receiver")

                              (:file       "bus-mixins")
                              (:file       "bus")

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

  :version     #.(version/string)
  :depends-on  ((:version "lift"                 "1.7.1")

                (:version "cl-rsb"               #.(version/string))
                (:version "rsb-transport-spread" #.(version/string))

                (:version "cl-rsb/test"          #.(version/string)))

  :components  ((:module     "spread"
                 :pathname   "test/transport/spread"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "fragmentation")
                              (:file       "connection")

                              (:file       "sender-receiver")

                              (:file       "connectors"))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string
                        "(network.spread.daemon:with-daemon (:port cl-rsb-system::*spread-port*)
                           (lift:run-tests :config (lift::lift-relative-pathname
                                                    \"lift-transport-spread.config\")))"))))
