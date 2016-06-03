;;;; builder.lisp --- Builder support for event class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.builder
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:architecture.builder-protocol

   #:rsb)

  (:export
   #:universal-builder-for-event-data)

  (:documentation
   "This package contains builder support for scopes and events."))

(cl:in-package #:rsb.builder)

;;; Builder support for scopes

(defmethod node-kind ((builder t) (node scope))
  'rsb:scope)

(defmethod node-relations ((builder t) (node scope))
  '((:component . *)))

(defmethod node-relation ((builder  t)
                          (relation (eql :component))
                          (node     scope))
  (scope-components node))

;;; Builder support for events

(defmethod node-kind ((builder t) (node event))
  'rsb:event)

(defmethod node-initargs ((builder t) (node event))
  `(:scope ,(event-scope node)
    ,@(when-let ((number (event-sequence-number node)))
        `(:sequence-number ,number))
    ,@(when-let ((id (event-id node)))
        `(:id ,id))
    ,@(when-let ((origin (event-origin node)))
        `(:origin ,origin))
    :method ,(event-method node)))

(defmethod node-relations ((builder t) (node event))
  '((:meta-data . (:map . :key))
    (:timestamp . (:map . :key))
    (:cause     . *)
    (:data      . 1)))

(let+ (((&flet handle-plist (plist)
          (loop :for (key target) :on plist :by #'cddr
             :collect target          :into targets
             :collect (list :key key) :into args
             :finally (return (values targets args))))))

  (defmethod node-relation ((builder  t)
                            (relation (eql :meta-data))
                            (node     event))
    (handle-plist (meta-data-plist node)))

  (defmethod node-relation ((builder  t)
                            (relation (eql :timestamp))
                            (node     event))
    (handle-plist (timestamp-plist node))))

(defmethod node-relation ((builder  t)
                          (relation (eql :cause))
                          (node     event))
  (event-causes node))

(defmethod node-relation ((builder  t)
                          (relation (eql :data))
                          (node     event))
  (event-data node))

;;; Utility functions

(declaim (ftype (function * (values function &optional))
                universal-builder-for-event-data))

(defun universal-builder-for-event-data ()
  "Return a \"peek function\"[1] that switches to the universal
   builder[2] for event data.

   [1] See `architecture.builder-protocol:walk-nodes'
   [2] See `architecture.builder-protocol.universal-builder' package."
  (let ((universal (make-instance 'architecture.builder-protocol.universal-builder:universal-builder)))
    (lambda (builder relation relation-args node)
      (if (eq relation :data)
          (values t nil nil nil universal)
          t))))
