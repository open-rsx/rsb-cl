;;;; rsb-converter-protocol-buffer.asd --- System containing converter for protocol buffer payloads.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (find-package '#:cl-rsb-system)
    (load (merge-pathnames "cl-rsb.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-system)

(defsystem :rsb-converter-protocol-buffer
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LGPLv3" ; see COPYING file for details.
  :description "This system provides a converter for Google protocol
                buffer payloads."
  :depends-on  ((:version :cl-rsb #.(version/string :revision? t))
                :cl-protobuf)
  :encoding    :utf-8
  :components  ((:module     "converter"
                 :pathname   "src/converter"
                 :components ((:file       "protocol-buffers")))))
