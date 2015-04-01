;;;; connection.lisp --- Spread connections with membership management.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
                   :accessor connection-%connection
                   :documentation
                   "The underlying Spread connection used by the
                    connector instance.")
   (groups         :type     hash-table
                   :reader   connection-%groups
                   :initform (make-hash-table :test #'equal)
                   :documentation
                   "A mapping of group names to reference counts.

                    The connection instance is a member of every group
                    that has a positive reference count.")
   (receive-buffer :type     (or null simple-octet-vector)
                   :accessor connection-%receive-buffer
                   :initform nil
                   :documentation
                   "Stores a buffer for receiving messages.

                    The buffer is allocated lazily to avoid wasting
                    memory for send-only connections."))
  (:documentation
   "Instances of this class represent connections to the Spread
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
    (setf (connection-%connection instance)
          (network.spread:connect name))))

(defmethod ref-group ((connection connection) (group string))
  (let+ (((&structure-r/o connection- %connection (groups %groups))
          connection))
    (when (= (incf (gethash group groups 0)) 1)
      (log:info "~@<~A is joining group ~A~@:>" connection group)
      (network.spread:join %connection group))))

(defmethod unref-group :around ((connection connection) (group string))
  (if (gethash group (connection-%groups connection))
      (call-next-method)
      (log:warn "~@<~A was asked to unreference unknown group ~A~@:>"
                connection group)))

(defmethod unref-group ((connection connection) (group string))
  (let+ (((&structure-r/o connection- %connection (groups %groups))
          connection))
    (when (zerop (decf (gethash group groups 0)))
      (log:info "~@<~A is leaving group ~A~@:>" connection group)
      (remhash group groups)
      (network.spread:leave %connection group))))

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
    (let+ (((&accessors-r/o (connection connection-%connection)
                            (buffer     %ensure-receive-buffer))
            connection))
      (with-spread-condition-translation
        (values buffer
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
        (connection-%connection connection) destination payload))))

(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let+ (((&structure-r/o connection- %connection %groups) object))
     (format stream "~A (~D)"
             (network.spread:connection-name %connection)
             (hash-table-count %groups)))))

;;; Utility functions

(defun %ensure-receive-buffer (connection)
  (let+ (((&structure connection- %receive-buffer) connection))
    (or %receive-buffer
        (setf %receive-buffer
              (make-octet-vector
               network.spread:+maximum-message-data-length+)))))
