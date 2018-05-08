;;;; rsb-converter-protocol-buffer.asd --- System containing converter for protocol buffer payloads.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem "rsb-converter-protocol-buffer"
  :description "Converter for Google protocol buffer payloads."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ((:version "cl-rsb" #.(version/string :revision? t))
                "cl-protobuf")

  :components  ((:module     "converter"
                 :pathname   "src/converter"
                 :components ((:file       "protocol-buffers")))))
