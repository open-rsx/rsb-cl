;;;; rsb-builder.asd --- Builder support for RSB objects.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-builder
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Builder support for RSB objects such as events."
  :depends-on  (:alexandria
                :let-plus

                (:version :architecture.builder-protocol "0.3")

                (:version :cl-rsb                        #.(version/string)))
  :components  ((:module     "src"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op :rsb-builder-test))))

(defsystem :rsb-builder-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-builder system."
  :depends-on  ((:version :lift                               "1.7.1")

                (:version :architecture.builder-protocol-test "0.3")

                (:version :rsb-builder                        #.(version/string))

                (:version :cl-rsb-test                        #.(version/string)))
  :components  ((:module     "test"
                 :components ((:file       "builder")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-builder-test))))
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-builder-test \"lift-builder.config\"))")))
