;;;; rsb-introspection.asd --- System definition for introspection for RSB.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-introspection"
  :description "Introspection support for RSB."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version"
                         "cl-protobuf")
  :depends-on  ("rsb-protocol-config"

                "utilities.print-items"
                (:version "uiop"                       "3") ; for portable platform information

                (:version "rsb"                        (:read-file-form "version-string.sexp"))
                (:version "rsb-model"                  (:read-file-form "version-string.sexp"))
                (:version "rsb-patterns-request-reply" (:read-file-form "version-string.sexp")))

  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   "data"
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

  :in-order-to ((test-op (test-op "rsb-introspection/test"))))

(defsystem "rsb-introspection/test"
  :description "Unit tests for the rsb-introspection system."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "lift"              "1.7.1")

                (:version "rsb-introspection" (:read-file-form "version-string.sexp"))

                (:version "rsb/test"          (:read-file-form "version-string.sexp"))
                (:version "rsb-model/test"    (:read-file-form "version-string.sexp")))

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
                               :if-feature :sbcl))))

  :perform     (test-op (operation component)
                 (eval (read-from-string "(log:config :warn)")) ; less noise
                 (eval (read-from-string "(lift:run-tests :config
                                            (asdf:system-relative-pathname
                                             :rsb-introspection/test \"lift-introspection.config\"))"))))
