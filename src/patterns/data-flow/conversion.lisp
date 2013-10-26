;;;; conversion.lisp --- Converters used in the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(macrolet
    ((define-data-flow-converter (condition-class
                                  wire-schema
                                  notification-class
                                  notification-which
                                  notification-peer)
       `(progn
         (defmethod rsb.converter:wire->domain?
             ((converter   (eql :data-flow))
              (wire-data   simple-array)
              (wire-schema (eql ,wire-schema)))
           (values :data-flow ',condition-class))

         (defmethod rsb.converter:domain->wire?
             ((converter     (eql :data-flow))
              (domain-object ,condition-class))
           (values :data-flow 'nibbles:octet-vector
                   ,wire-schema))

         (defmethod rsb.converter:wire->domain
             ((converter   (eql :data-flow))
              (wire-data   simple-array)
              (wire-schema (eql ,wire-schema)))
           (let ((raw (pb:unpack wire-data ',notification-class)))
             (make-condition ',condition-class
                             :which (make-keyword (,notification-which raw))
                             :peer  (uuid:byte-array-to-uuid
                                     (,notification-peer raw)))))

         (defmethod rsb.converter:domain->wire
             ((converter     (eql :data-flow))
              (domain-object ,condition-class))
           (values (pb:pack* (make-instance
                              ',notification-class
                              :which (string (which domain-object))
                              :peer  (uuid:uuid-to-byte-array
                                      (peer domain-object))))
                   ,wire-schema)))))

  (define-data-flow-converter
    high-watermark-reached/remote
    :|.rst.rsb.dataflow.HighWatermarkReached|
    rst.rsb.dataflow:high-watermark-reached
    rst.rsb.dataflow:high-watermark-reached-which
    rst.rsb.dataflow:high-watermark-reached-peer)
  (define-data-flow-converter
    low-watermark-reached/remote
    :|.rst.rsb.dataflow.LowWatermarkReached|
    rst.rsb.dataflow:low-watermark-reached
    rst.rsb.dataflow:low-watermark-reached-which
    rst.rsb.dataflow:low-watermark-reached-peer))
