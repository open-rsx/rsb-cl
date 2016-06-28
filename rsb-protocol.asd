;;;; rsb-protocol.asd --- System for loading the native RSB communication protocol.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-protocol"
  :description "Google protocol buffer-based communication protocol."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version"
                         "cl-protobuf")
  :depends-on  ("rsb-protocol-config")

  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   data
                 :serial     t
                 :components ((:file       "EventId"
                               :pathname   "rsb/protocol/EventId")
                              (:file       "EventMetaData"
                               :pathname   "rsb/protocol/EventMetaData")
                              (:file       "Notification"
                               :pathname   "rsb/protocol/Notification")
                              (:file       "FragmentedNotification"
                               :pathname   "rsb/protocol/FragmentedNotification")

                              (:file       "EventsByScopeMap"
                               :pathname   "rsb/protocol/collections/EventsByScopeMap")))))
