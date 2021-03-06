;;;; protocol-buffers.lisp --- Converter for protocol buffer wire schemas.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

(defmethod wire->domain? ((converter   (eql :protocol-buffer))
                          (wire-data   simple-array)
                          (wire-schema symbol))
  (when-let* ((descriptor (pb:find-descriptor wire-schema :error? nil))
              (class      (pb:descriptor-class descriptor :error? nil)))
    (let ((classes (list (load-time-value (find-class 'simple-array)) class)))
      (declare (dynamic-extent classes))
      (when (c2mop:compute-applicable-methods-using-classes
             #'pb:unpack classes)
        (values converter (class-name class))))))

(defmethod domain->wire? ((converter     (eql :protocol-buffer))
                          (domain-object standard-object))
  (let* ((class   (class-of domain-object))
         (classes (list class)))
    (declare (dynamic-extent classes))
    (when (c2mop:compute-applicable-methods-using-classes
           #'pb:pack classes)
      (values converter 'octet-vector (class-name class)))))

(defmethod wire->domain ((converter   (eql :protocol-buffer))
                         (wire-data   simple-array)
                         (wire-schema symbol))
  (check-type wire-data octet-vector)

  (let* ((descriptor (pb:find-descriptor (string wire-schema)))
         (class      (pb:descriptor-class descriptor)))
    (nth-value 0 (pb:unpack wire-data class))))

(defmethod domain->wire ((converter     (eql :protocol-buffer))
                         (domain-object standard-object))
  (let* ((descriptor  (pb:message-descriptor domain-object))
         (wire-schema (intern (pb:descriptor-qualified-name descriptor)
                              :keyword)))
   (values (pb:pack* domain-object) wire-schema)))
