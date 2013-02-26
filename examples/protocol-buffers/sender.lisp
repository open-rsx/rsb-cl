;;;; sender.lisp --- An example program demonstrating the protocol buffer converter.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; Note: depending on your RSB configuration, this example may require
;; a running Spread daemon for successful execution.

;; In order to be able to use a protocol buffer data definition, a
;; data-holder and corresponding (de)serialization code has to be
;; generated. When a textual definition is available, this can be done
;; at runtime like this:
#.(let ((descriptor (pbf:load/text (merge-pathnames
                                    "Image.proto"
                                    *compile-file-pathname*))))
    (pbb:emit descriptor :class)
    (pbb:emit descriptor :packed-size)
    (pbb:emit descriptor :serializer)
    (pbb:emit descriptor :deserializer)
    (values))

;; Once the data-holder class and its (de)serialization methods have
;; been generated, any participant that is configured to use the
;; generic protocol buffer converter will be able to send and receive
;; events containing the protocol buffer message in question as a
;; payload:
(rsb:with-participant (informer :informer "/example/protobuf")
  (rsb:send informer (make-instance 'example:image
                                    :width  100
                                    :height 100
                                    :data   (nibbles:octet-vector 1 2 3))))
;; mark-end::body
