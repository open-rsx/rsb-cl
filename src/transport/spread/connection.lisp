;;;; connection.lisp --- Spread connections with membership management.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Spread connection protocol

(defgeneric ref-group (connection group)
  (:documentation
   "Increase the reference count of GROUP, causing CONNECTION to join
GROUP in case of a 0 -> 1 transition."))

(defgeneric unref-group (connection group)
  (:documentation
   "Decrease the reference count of GROUP, causing CONNECTION to leave
GROUP in case of a 1 -> 0 transition."))

(defgeneric receive-message (connection block?)
  (:documentation
   "Receive and return a message from CONNECTION.

    BLOCK? controls whether the call should block until a message has
    been received."))

(defgeneric send-message (connection destination payload)
  (:documentation
   "Send the PAYLOAD to DESTINATION via CONNECTION in a Spread
    message."))

;;; `connection' class

(defclass connection ()
  ((connection     :initarg  :connection
                   :type     network.spread:connection
                   :documentation
                   "The underlying spread connection used by the connector
instance.")
   (groups         :type     hash-table
                   :initform (make-hash-table :test #'equal)
                   :documentation
                   "A mapping of group names to reference counts. The
connection instance is a member of every group that has a positive
reference count.")
   (receive-buffer :type     (or null simple-octet-vector)
                   :initform nil
                   :documentation
                   "Stores a buffer which is used for receiving
messages. The buffer is allocated lazily to avoid wasting memory for
send-only connections."))
  (:documentation
   "Instances of this class represent connections to Spread
communication system and maintain a list of the Spread multicast
groups in which they are members."))

(defmethod initialize-instance :before ((instance connection)
                                        &key
                                        connection
                                        name)
  (unless (or name connection)
    (missing-required-initarg 'connector :either-name-or-connection)))

(defmethod shared-initialize :before ((instance   connection)
                                      (slot-names t)
                                      &key
                                      connection
                                      name)
  (when (and name connection)
    (incompatible-initargs 'connection
                           :name       name
                           :connection connection)))

(defmethod shared-initialize :after ((instance   connection)
                                     (slot-names t)
                                     &key
                                     name)
  (when name
    (setf (slot-value instance 'connection)
          (network.spread:connect name))))

(defmethod ref-group ((connection connection) (group string))
  (when (= (incf (gethash group (slot-value connection 'groups) 0)) 1)
    (log:info "~@<~A is joining group ~A~@:>"
              connection group)
    (network.spread:join (slot-value connection 'connection) group)))

(defmethod unref-group :around ((connection connection) (group string))
  (if (gethash group (slot-value connection 'groups))
      (call-next-method)
      (log:warn "~@<~A was asked to unreference unknown group ~A~@:>"
                connection group)))

(defmethod unref-group ((connection connection) (group string))
  (when (zerop (decf (gethash group (slot-value connection 'groups) 0)))
    (log:info "~@<~A is leaving group ~A~@:>" connection group)
    (remhash group (slot-value connection 'groups))
    (network.spread:leave (slot-value connection 'connection) group)))

(macrolet
    ((with-spread-condition-translation (&body body)
       `(handler-bind
            ((network.spread:spread-error
               (lambda (condition)
                 (when (member (network.spread:spread-error-code condition)
                               '(:net-error-on-session :connection-closed))
                   (error 'connection-unexpectedly-closed
                          :connection connection
                          :cause      condition)))))
          ,@body)))

  (defmethod receive-message ((connection connection) (block? t))
    (declare (inline %ensure-receive-buffer))
    (let+ ((buffer (%ensure-receive-buffer connection))
           ((&slots-r/o connection) connection))
      (with-spread-condition-translation
        (values
         buffer
         (network.spread:receive-into connection buffer
                                      :block?         block?
                                      :return-sender? nil
                                      :return-groups? nil)))))

  (defmethod send-message ((connection  connection)
                           (destination list)
                           (payload     simple-array))
    (check-type payload simple-octet-vector "a simple-octet-vector")

    (with-spread-condition-translation
        (network.spread:send-bytes
         (slot-value connection 'connection) destination payload))))

(defmethod print-object ((object connection) stream)
  (with-slots (connection groups) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A (~D)"
              (network.spread:connection-name connection)
              (hash-table-count groups)))))

;;; Utility functions

(defun %ensure-receive-buffer (connection)
  (or (slot-value connection 'receive-buffer)
      (setf (slot-value connection 'receive-buffer)
            (make-octet-vector
             network.spread:+maximum-message-data-length+))))
