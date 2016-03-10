;;;; connector.lisp --- Superclass for connector classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; `connector' class

(defclass connector (uri-mixin
                     error-policy-mixin)
  ((rsb::uri :reader   connector-url))
  (:documentation
   "A connector implements access to the bus by means of a particular
mechanism. One example is a connector that makes use of the Spread
group communication framework."))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
                                     &key
                                     schema
                                     host
                                     port
                                     &allow-other-keys)
  (setf (slot-value instance 'rsb::uri)
        (make-instance 'puri:uri
                       :scheme schema
                       :host   (or host (when port "localhost"))
                       :port   port)))

(defmethod connector-relative-url ((connector connector)
                                   (uri       puri:uri))
  (puri:merge-uris uri (connector-url connector)))

(defmethod connector-relative-url ((connector connector)
                                   (thing     string))
  (connector-relative-url connector (make-scope thing)))

(defmethod connector-relative-url ((connector connector)
                                   (thing     t))
  (connector-relative-url connector (relative-url thing)))

(defmethod print-items:print-items append ((object connector))
  `((:direction ,(connector-direction object)        "~A")
    (:url       ,(connector-relative-url object "/") " ~A"
     ((:after :direction)))))

(defmethod print-object ((object connector) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (t
     (print-unreadable-object (object stream :identity t)
       (print-items:format-print-items
        stream (print-items:print-items object))))))
