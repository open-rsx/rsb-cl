;;;; rsb-config.asd --- Configuration of the rsb system.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-config"
  :description "Configuration of the rsb system."
  :depends-on  ("uiop"
                "alexandria"
                "cl-ppcre"
                "inferior-shell")
  :components  ((:module "config"
                 :serial t
                 :components ((:file "package")
                              (:file "git")
                              (:file "files")))))
