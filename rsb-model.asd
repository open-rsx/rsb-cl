;;;; rsb-model.asd --- System definition for model for RSB.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-model
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Modeling of and inference on RSB systems."
  :depends-on  (:alexandria
                :let-plus
                :more-conditions
                :utilities.print-items

                (:version :cl-rsb #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "model"
                 :pathname   "src/model"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "classes")))

                (:module     "model-inference"
                 :pathname   "src/model/inference"
                 :depends-on ("model")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "util")
                              (:file       "inference"))))

  :in-order-to ((test-op (test-op :rsb-model-test))))

(defsystem :rsb-model-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-model system."
  :depends-on  ((:version :lift        "1.7.1")

                (:version :rsb-model   #.(version/string))

                (:version :cl-rsb-test #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "model"
                 :pathname   "test/model"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "classes")))

                (:module     "model-inference"
                 :pathname   "test/model/inference"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "inference")))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-model-test))))
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-model-test \"lift-model.config\"))")))
