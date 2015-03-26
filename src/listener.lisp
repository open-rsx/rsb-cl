;;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass listener (receiving-client)
  ((direction :allocation :class
              :initform :in-push)
   (handlers  :type     list
              :accessor rsb.ep:handlers
              :initform '()
              :documentation
              "Stores the list of handlers two which events received
               by this listener should be dispatched."))
  (:documentation
   "Pushes events received on a channel to handlers (push style).

    Consists of a set of filters, a set of handlers and has a
    mechanism for dispatching matching events to these handlers."))

(register-participant-class 'listener)

(defmethod shared-initialize :after ((instance   listener)
                                     (slot-names t)
                                     &key
                                     (handlers '() handlers-supplied?))
  (when handlers-supplied?
    (setf (rsb.ep:handlers instance) handlers)))

(defmethod (setf rsb.ep:handlers) :around ((new-value list)
                                           (listener  listener))
  (let* ((configurator (rsb.ep:client-configurator listener))
         (old-value    (rsb.ep:handlers listener))
         (added        (set-difference new-value old-value))
         (removed      (set-difference old-value new-value)))
    (prog1
        (call-next-method)
      (log:debug "~@<~A~@:_~
                    ~2@Tadded   handlers ~:A~@:_~
                    ~2@Tremoved handlers ~:A~
                  ~:>"
                 listener added removed)
      (iter (for handler in added)
            (rsb.ep:notify configurator handler :handler-added))
      (iter (for handler in removed)
            (rsb.ep:notify configurator handler :handler-removed)))))
