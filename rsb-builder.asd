;;;; rsb-builder.asd --- Builder support for RSB objects.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:rsb-system)
    (load (merge-pathnames "rsb.asd" *load-truename*))
    (values))

(cl:in-package #:rsb-system)

(defsystem "rsb-builder"
  :description "Builder support for RSB objects such as events."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("alexandria"
                "let-plus"

                (:version "architecture.builder-protocol"                   "0.3")
                (:version "architecture.builder-protocol.universal-builder" "0.3")

                (:version "rsb"                                             #.(version/string)))

  :components  ((:module     "src"
                 :components ((:file       "builder"))))

  :in-order-to ((test-op (test-op "rsb-builder/test"))))

(defsystem "rsb-builder/test"
  :description "Unit tests for the rsb-builder system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "lift"                               "1.7.1")

                (:version "architecture.builder-protocol/test" "0.3")

                (:version "rsb-builder"                        #.(version/string))

                (:version "rsb/test"                           #.(version/string)))

  :components  ((:module     "test"
                 :components ((:file       "builder"))))

  :perform (test-op (operation component)
             (eval (read-from-string "(lift:run-tests :config
                                        (asdf:system-relative-pathname
                                         :rsb-builder/test \"lift-builder.config\"))"))))
