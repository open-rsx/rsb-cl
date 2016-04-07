;;;; rsb-introspection.asd --- System definition for introspection for RSB.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-introspection
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Introspection support for RSB."
  :defsystem-depends-on (:cl-protobuf)
  :depends-on  (:utilities.print-items
                (:version :uiop                       "3") ; for portable platform information

                (:version :cl-rsb                     #.(version/string))
                (:version :rsb-model                  #.(version/string))
                (:version :rsb-patterns-request-reply #.(version/string)))
  :encoding    :utf-8
  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   #.+protocol-directory+
                 :components ((:file       "Hello"
                               :pathname   "rsb/protocol/introspection/Hello")
                              (:file       "Bye"
                               :pathname   "rsb/protocol/introspection/Bye")))

                (:module     "introspection"
                 :pathname   "src/introspection"
                 :depends-on ("protocol")
                 :serial     t
                 :components ((:file       "package")

                              ;; Platform information interface.
                              (:file       "platform-common")
                              (:file       "platform-sbcl-linux"
                               :if-feature (:and :sbcl :linux))
                              (:file       "platform-sbcl-darwin"
                               :if-feature (:and :sbcl :darwin))
                              (:file       "platform-sbcl-win32"
                               :if-feature (:and :sbcl :win32))
                              (:file       "platform-generic"
                               :if-feature (:or (:not :sbcl)
                                                (:and (:not :linux)
                                                      (:not :darwin)
                                                      (:not :win32))))

                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "variables")
                              (:file       "model")
                              (:file       "conversion")

                              (:file       "mixins")
                              (:file       "local-introspection")
                              (:file       "timing-tracking")
                              (:file       "remote-introspection")

                              (:file       "reloading"
                               :if-feature :sbcl))))

  :in-order-to ((test-op (test-op :rsb-introspection-test))))

(defsystem :rsb-introspection-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "Unit tests for the rsb-introspection system."
  :depends-on  ((:version :lift              "1.7.1")

                (:version :rsb-introspection #.(version/string))

                (:version :cl-rsb-test       #.(version/string))
                (:version :rsb-model-test    #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "introspection"
                 :pathname   "test/introspection"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "platform")

                              (:file       "model")

                              (:file       "local-introspection")
                              (:file       "timing-tracking")
                              (:file       "remote-introspection")

                              (:file       "reloading"
                               :if-feature :sbcl)))))

(defmethod perform ((op test-op) (system (eql (find-system :rsb-introspection-test))))
  (eval (read-from-string "(log:config :warn)")) ; less noise
  (eval (read-from-string "(lift:run-tests :config
                             (asdf:system-relative-pathname
                              :rsb-introspection-test \"lift-introspection.config\"))")))
