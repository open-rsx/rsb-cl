;;;; rsb-model-builder.asd --- Builder support for RSB model objects.
;;;;
;;;; Copyright (C) 2015, 2016, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:rsb-system)
    (load (merge-pathnames "rsb.asd" *load-truename*))
    (values))

(cl:in-package #:rsb-system)

(defsystem "rsb-model-builder"
  :description "Builder support for RSB model objects"
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "architecture.builder-protocol" "0.3")

                (:version "rsb-model"                     #.(version/string))
                (:version "rsb-introspection"             #.(version/string)))

  :components  ((:module   "model"
                 :pathname "src/model"
                 :components ((:file       "builder")))

                (:module   "introspection"
                 :pathname "src/introspection"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op "rsb-model-builder/test"))))

(defsystem "rsb-model-builder/test"
  :description "Unit tests for the rsb-model-builder system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "fiveam"                             "1.4")

                (:version "architecture.builder-protocol/test" "0.3")

                (:version "rsb-model-builder"                  #.(version/string))

                (:version "rsb/test"                           #.(version/string)))
  :components  ((:module     "model"
                 :pathname   "test/model"
                 :components ((:file       "builder")))

                (:module     "introspection"
                 :pathname   "test/introspection"
                 :components ((:file       "builder"))))

  :perform     (test-op (operation component)
                 (symbol-call '#:rsb.model.builder.test '#:run-tests)))
