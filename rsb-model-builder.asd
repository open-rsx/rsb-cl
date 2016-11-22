;;;; rsb-model-builder.asd --- Builder support for RSB model objects.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-model-builder
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Builder support for RSB model objects"
  :depends-on  ((:version :architecture.builder-protocol "0.3")

                (:version :rsb-model                     #.(version/string))
                (:version :rsb-introspection             #.(version/string)))
  :components  ((:module   "model"
                 :pathname "src/model"
                 :components ((:file       "builder")))

                (:module   "introspection"
                 :pathname "src/introspection"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op :rsb-model-builder/test))))

(defsystem :rsb-model-builder/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-model-builder system."
  :depends-on  ((:version :lift                               "1.7.1")

                (:version :architecture.builder-protocol/test "0.3")

                (:version :rsb-model-builder                  #.(version/string))

                (:version :cl-rsb/test                        #.(version/string)))
  :components  ((:module     "model"
                 :pathname   "test/model"
                 :components ((:file       "builder")))

                (:module     "introspection"
                 :pathname   "test/introspection"
                 :components ((:file       "builder")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-model-builder/test))))
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-model-builder/test \"lift-model-builder.config\"))")))
