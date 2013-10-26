;;;; rsb-data-flow.asd --- TODO.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-data-flow-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:rsb-data-flow-system)

#.(progn
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(defsystem :rsb-data-flow
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(cl-rsb-system:version/string)
  :license     "LGPLv3; see COPYING file for details."
  :description "This system provides TODO"
  :depends-on  ((:version :cl-rsb                 #.(cl-rsb-system:version/string :commit? t))
                (:version :cl-rsb-and-cl-protobuf #.(cl-rsb-system:version/string :commit? t)))
  :encoding    :utf-8
  :components  ((:protocol-buffer-descriptor-directory "protocol"
                 :pathname   "../../rst/rst-proto/proto/sandbox/" #+later +protocol-directory+
                 :components ((:file       "HighWatermarkReached"
                               :pathname   "rst/rsb/dataflow/HighWatermarkReached")
                              (:file       "LowWatermarkReached"
                               :pathname   "rst/rsb/dataflow/LowWatermarkReached")))

                (:module     "data-flow"
                 :pathname   "src/patterns/data-flow"
                 :depends-on ("protocol")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "protocol")
                              (:file       "conditions")
                              (:file       "conversion")
                              (:file       "mixins")
                              (:file       "macros")
                              (:file       "source")
                              (:file       "sink")))))
