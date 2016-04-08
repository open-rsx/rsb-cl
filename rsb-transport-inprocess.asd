;;;; rsb-transport-inprocess.asd --- System containing the inprocess transport.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:rsb-system)
    (load (merge-pathnames "rsb.asd" *load-truename*))
    (values))

(cl:in-package #:rsb-system)

(defsystem "rsb-transport-inprocess"
  :description "Simple and efficient in-process transport."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "rsb" #.(version/string :revision? t)))

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

  :version     #.(version/string)
  :depends-on  ((:version "fiveam"                  "1.4")

                (:version "rsb-transport-inprocess" #.(version/string))

                (:version "rsb/test"                #.(version/string)))

  :components  ((:module     "inprocess"
                 :pathname   "test/transport/inprocess"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connectors"))))

  :perform     (test-op (operation component)
                 (symbol-call '#:log '#:config :warn) ; less noise
                 (symbol-call '#:rsb.transport.inprocess.test '#:run-tests)))
