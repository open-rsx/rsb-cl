;;;; rsb-transport-inprocess.asd --- System containing the inprocess transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-transport-inprocess
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a simple inprocess transport."
  :depends-on  ((:version :cl-rsb #.(version/string :revision? t)))
  :encoding    :utf-8
  :components  ((:module     "inprocess"
                 :pathname   "src/transport/inprocess"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "transport")
                              (:file       "connectors"))))

  :in-order-to ((test-op (test-op :rsb-transport-inprocess/test))))

(defsystem :rsb-transport-inprocess/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit Tests for the rsb-transport-inprocess system."
  :depends-on  ((:version :lift                 "1.7.1")

                (:version :rsb-transport-inprocess #.(version/string))

                (:version :cl-rsb/test          #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "inprocess"
                 :pathname   "test/transport/inprocess"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connectors")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-transport-inprocess/test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string
         "(lift:run-tests :config (lift::lift-relative-pathname
                                   \"lift-transport-inprocess.config\"))")))
