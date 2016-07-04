;;;; builder.lisp --- (un)builder support for model classes.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.builder
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:architecture.builder-protocol

   #:rsb.model)

  (:shadowing-import-from #:rsb.model
   #:node))

(cl:in-package #:rsb.model.builder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-node-methods ((class &key (kind (make-keyword class)))
                                 &rest slots)
    (let+ ((conc-name (symbolicate class '#:-))
           (slots     (mapcar #'ensure-list slots))
           ((&flet make-node-initargs (builder)
              (let+ (((&flet+ maybe-initarg ((name &key (builders '(t))))
                        (when (or (eq builders t) (member builder builders
                                                          :test #'equal))
                          (let ((accessor (symbolicate conc-name name)))
                            `(,(make-keyword name) (,accessor node)))))))
                `(defmethod node-initargs ((builder ,builder) (node ,class))
                   (list ,@(mapcan #'maybe-initarg slots)))))))
      `(progn
         (defmethod node-kind ((builder t) (node ,class))
           ,kind)
         ,(make-node-initargs t)
         ,(make-node-initargs '(eql :reverse))))))

;;; generic node

(defmethod node-kind ((builder t) (node node))
  (node-kind builder (node-info node)))

(defmethod node-initargs ((builder t) (node node))
  (node-initargs builder (node-info node)))

(defmethod node-relations ((builder t) (node node))
  (append (list* '(:children . *) (node-relations builder (node-info node)))
          (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :children))
                          (node     node))
  (node-children node))

(defmethod node-relation ((builder t) (relation t) (node node))
  (node-relation builder relation (node-info node)))

;;; `participant-node' and `participant-info'

(define-node-methods (participant-info :kind :participant)
  (kind      :builders t)
  (id        :builders t)
  scope
  type)

(defmethod node-relations ((builder t) (node participant-info))
  (list* '(:transports . *) (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :transports))
                          (node     participant-info))
  (participant-info-transports node))

;;; `process-node' and `[remote-]process-info'

(define-node-methods (process-info :kind :process)
  (process-id     :builders t)
  (program-name   :builders t)
  start-time
  executing-user
  rsb-version
  (display-name   :builders t))

(defmethod node-relations ((builder t) (node process-info))
  (list* '(:commandline-arguments . *) (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :commandline-arguments))
                          (node     process-info))
  (process-info-commandline-arguments node))

(defmethod node-initargs ((builder t) (node remote-process-info))
  (list* :state (process-info-state node) (call-next-method)))

(defmethod node-relations ((builder t) (node remote-process-info))
  (list* '(:transports . *) (call-next-method)))

(defmethod node-relation ((builder  t)
                          (relation (eql :transports))
                          (node     remote-process-info))
  (process-info-transports node))

;;; `host-node' and `[remote-]host-info'

(define-node-methods (host-info :kind :host)
  (id              :builders t)
  (hostname        :builders t)
  machine-type
  machine-version
  software-type
  software-version)

(defmethod node-initargs ((builder t) (node remote-host-info))
  (list* :state (host-info-state node) (call-next-method)))

;;; reverse builder
;;;
;;; Traverses tree from children to parents and only included basic
;;; information in each node.

(defmethod node-relations :around ((builder (eql :reverse))
                                   (node    node))
  (when-let ((parent (node-parent node)))
    '((:parent . 1))))

(defmethod node-relation ((builder  (eql :reverse))
                          (relation (eql :parent))
                          (node     node))
  (node-parent node))
