;;;; builder.lisp --- (un)builder support for introspection nodes.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection.builder
  (:use
   #:cl
   #:let-plus

   #:architecture.builder-protocol

   #:rsb.introspection)

  (:import-from #:rsb.introspection
   #:tracked-quantity
   #:tracked-quantity-name
   #:tracked-quantity-value
   #:tracked-quantity-history

   #:timing-tracker-%clock-offset
   #:timing-tracker-%latency

   #:entry-%tracker

   #:remote-introspection-database)

  (:import-from #:rsb.model.builder
   #:define-node-methods))

(cl:in-package #:rsb.introspection.builder)

;;; database

(defmethod node-kind ((builder t) (node remote-introspection-database))
  :database)

(defmethod node-relations ((builder t) (node remote-introspection-database))
  '((:children . *)))

(defmethod node-relation ((builder  t)
                          (relation (eql :children))
                          (node     remote-introspection-database))
  (introspection-hosts node))

;;; `host-entry'

(defmethod node-relations ((builder t) (node host-entry))
  (list* '(:clock-offset . 1) '(:latency . 1) (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :clock-offset))
                          (node     host-entry))
  (timing-tracker-%clock-offset (entry-%tracker node)))

(defmethod node-relation ((builder  t)
                          (relation (eql :latency))
                          (node     host-entry))
  (timing-tracker-%latency (entry-%tracker node)))

;;; `process-entry'

(defmethod node-relations ((builder t) (node process-entry))
  (list* '(:latency . 1) (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :latency))
                          (node     process-entry))
  (timing-tracker-%latency (entry-%tracker node)))

;;; `tracked-quantity'

(define-node-methods (tracked-quantity :kind :tracked-quantity)
  (name  :builders t)
  (value :builders t))

(defmethod node-relations ((builder t) (node tracked-quantity))
  '((:history . *)))

(defmethod node-relation ((builder  t)
                          (relation (eql :history))
                          (node     tracked-quantity))
  (tracked-quantity-history node))
