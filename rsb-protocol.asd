;;;; rsb-protocol.asd --- System for loading the native RSB communication protocol.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-protocol
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Google protocol buffer-based communication protocol."
  :defsystem-depends-on (:cl-protobuf)
  :encoding    :utf-8
  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   #.+protocol-directory+
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
