;;;; bus-mixins.lisp --- Mixins for bus classes.
;;;;
;;;; Copyright (C) 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `connector-container-mixin'

(defclass connector-container-mixin ()
  ((connectors      :type     list
                    :accessor %connectors
                    :initform '()
                    :documentation
                    "Stores a list of local connectors connected to
                     the bus.")
   (connectors-lock :reader   %connectors-lock
                    :initform (bt:make-recursive-lock "Bus Connectors Lock")
                    :documentation
                    "Stores a lock that can be used to protect the
                     connector list of the bus from concurrent
                     modification."))
  (:documentation
   "Manages a set of connectors attached to a bus."))

(defmethod (setf %connectors) :around ((new-value list)
                                       (bus       connector-container-mixin))
  (let ((old-value (%connectors bus)))
    (prog1
        (call-next-method)
      (cond
        ((and old-value (not new-value))
         (log:info "~@<~A has no more connectors~@:>" bus)
         (notify bus :connectors :detached))
        ((and (not old-value) new-value)
         (log:info "~@<~A got its first connector~@:>" bus)
         (notify bus :connectors :attached))))))

(defmethod print-items:print-items append ((object connector-container-mixin))
  `((:connector-count ,(length (%connectors object)) "(C ~D)")))

(defmethod notify ((recipient rsb.transport:connector)
                   (subject   connector-container-mixin)
                   (action    (eql :attached)))
  (log:debug "~@<Connector ~A is attaching to bus provider ~A~@:>"
             recipient subject)
  (bt:with-recursive-lock-held ((%connectors-lock subject))
    (push recipient (%connectors subject))))

(defmethod notify ((recipient rsb.transport:connector)
                   (subject   connector-container-mixin)
                   (action    (eql :detached)))
  (log:debug "~@<Connector ~A is detaching from bus provider ~A~@:>"
             recipient subject)
  (bt:with-recursive-lock-held ((%connectors-lock subject))
    (removef (%connectors subject) recipient)))
