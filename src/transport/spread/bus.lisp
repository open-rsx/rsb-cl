;;;; bus.lisp --- Bus implementation for the Spread transport.
;;;;
;;;; Copyright (C) 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `bus'

(defclass bus (connector-container-mixin
               sink-dispatcher-mixin
               print-items:print-items-mixin)
  ((connection :initarg  :connection
               :reader   connection
               :documentation
               "Stores a connection to a Spread daemon.")
   (receiver   :accessor %receiver
               :documentation
               "Stores a message receiver object.")
   (sender     :accessor %sender
               :documentation
               "Stores a message sender object."))
  (:default-initargs
   :connection (missing-required-initarg 'bus :connection))
  (:documentation
   "Manages a connection to a remote Spread daemon and local connectors."))

(defmethod initialize-instance :after ((instance bus)
                                       &key
                                       connection
                                       age-limit)
  (let+ (((&flet error-policy (condition)
            (bt:with-lock-held ((%connectors-lock instance))
              (map nil (rcurry #'apply-error-policy condition)
                   (%connectors instance))))))
    (setf (%receiver instance) (make-instance 'message-receiver
                                              :connection   connection
                                              :handlers     (list instance)
                                              :error-policy #'error-policy
                                              :age-limit    age-limit)
          (%sender instance)   (make-instance 'message-sender
                                              :connection connection))))

;;; State management

(defmethod notify ((recipient bus)
                   (subject   (eql t))
                   (action    (eql :attached)))
  (notify (%receiver recipient) subject action))

(defmethod notify ((recipient bus)
                   (subject   (eql t))
                   (action    (eql :detached)))
  (notify (%receiver recipient) subject :detaching)
  (detach (connection recipient))
  (notify (%receiver recipient) subject action))

(defmethod notify ((recipient bus)
                   (subject   t)
                   (action    subscribed))
  (call-next-method)
  (notify (%receiver recipient) (subscription-scope action) :attached))

(defmethod notify ((recipient bus)
                   (subject   t)
                   (action    unsubscribed))
  (notify (%receiver recipient) (subscription-scope action) :detached)
  (call-next-method))

(defmethod notify ((recipient bus)
                   (subject   (eql :connectors))
                   (action    t))
  (notify recipient t action))

;;; Outgoing notifications

(defmethod handle ((sink bus) (data outgoing-notification))
  (let* ((scope           (outgoing-notification-scope data))
         (scope-and-event (scope-and-event scope data)))
    (declare (dynamic-extent scope-and-event))
    (dispatch sink scope-and-event))

  (handle (%sender sink) data))

;;; Incoming notifications

(defmethod handle ((processor bus) (event incoming-notification))
  (let* ((scope           (make-scope
                           (bytes->string
                            (notification-scope
                             (incoming-notification-notification event)))))
         (scope-and-event (rsb.ep:scope-and-event scope event)))
    (declare (dynamic-extent scope-and-event))
    (dispatch processor scope-and-event)))
