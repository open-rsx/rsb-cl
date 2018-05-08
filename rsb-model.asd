;;;; rsb-model.asd --- System definition for model for RSB.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem "rsb-model"
  :description "Modeling of and inference on RSB systems."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("alexandria"
                "let-plus"
                "more-conditions"
                "utilities.print-items"

                (:version "cl-rsb" #.(version/string)))

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

  :in-order-to ((test-op (test-op "rsb-model/test"))))

(defsystem "rsb-model/test"
  :description "Unit tests for the rsb-model system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "lift"        "1.7.1")

                (:version "rsb-model"   #.(version/string))

                (:version "cl-rsb/test" #.(version/string)))

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
                              (:file       "inference"))))

  :perform     (test-op (component operation)
                 (eval (read-from-string "(lift:run-tests :config
                                           (asdf:system-relative-pathname
                                            :rsb-model/test \"lift-model.config\"))"))))
