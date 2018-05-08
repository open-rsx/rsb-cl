;;;; rsb-patterns-request-reply.asd --- System definition for rsb-patterns-request-reply system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-patterns-request-reply
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Request-Reply communication pattern for RSB."
  :depends-on  (:alexandria
                :iterate
                (:version :let-plus                      "0.2")
                :more-conditions
                (:version :utilities.print-items         "0.1")

                (:version :bordeaux-threads              "0.8.4")
                :closer-mop

                (:version :cl-rsb                        #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "patterns-request-reply"
                 :pathname   "src/patterns/request-reply"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "future")

                              (:file       "server")
                              (:file       "local-server")
                              (:file       "remote-server")

                              (:file       "macros"))))

  :in-order-to ((test-op (test-op :rsb-patterns-request-reply/test))))

(defsystem :rsb-patterns-request-reply/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-patterns-request-reply system."
  :depends-on  ((:version :lift                       "1.7.1")

                (:version :rsb-patterns-request-reply #.(version/string))
                (:version :rsb-transport-inprocess    #.(version/string))

                (:version :cl-rsb/test                #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "patterns-request-reply"
                 :pathname   "test/patterns/request-reply"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "future")

                              (:file       "server")
                              (:file       "local-server")
                              (:file       "remote-server")

                              (:file       "macros")

                              (:file       "integration")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-patterns-request-reply/test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-patterns-request-reply/test \"lift-patterns-request-reply.config\"))")))
