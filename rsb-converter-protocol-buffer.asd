;;;; rsb-converter-protocol-buffer.asd --- System containing converter for protocol buffer payloads.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-converter-protocol-buffer"
  :description "Converter for Google protocol buffer payloads."
  :license     "LGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :defsystem-depends-on ("rsb-version")
  :depends-on  ((:version "rsb" (:read-file-form "version-string.sexp"))
                "cl-protobuf")

  :components  ((:module     "converter"
                 :pathname   "src/converter"
                 :components ((:file       "protocol-buffers")))))
