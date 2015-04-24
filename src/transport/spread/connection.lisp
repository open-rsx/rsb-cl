;;;; connection.lisp --- Spread connections with membership management.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

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

(defmethod disconnect ((connection connection))
  (network.spread:disconnect (connection-%connection connection)))

(defmethod ref-group ((connection connection) (group string)
                      &key (waitable? nil))
  (let+ (((&structure-r/o connection- %connection (groups %groups))
          connection)
         ((&flet join ()
            (log:info "~@<~A is joining group ~A~@:>" connection group)
            (network.spread:join %connection group)))
         ((&flet join/waitable ()
            (let+ ((hook    (hooks:object-hook
                             %connection 'network.spread:join-hook))
                   (promise (lparallel:promise))
                   ((&labels notify-and-remove (joined-group members)
                      (log:debug "~@<~A got join notification for ~
                                  group ~S with members ~S.~@:>"
                                 connection joined-group members)
                      (when (and (string= joined-group group :end2 31)
                                 (member (network.spread:connection-name
                                          %connection)
                                         members :test #'string=))
                        (hooks:remove-from-hook hook #'notify-and-remove)
                        (lparallel:fulfill promise)))))
              (hooks:add-to-hook hook #'notify-and-remove)
              (join)
              promise)))
         (new-value (incf (gethash group groups 0))))
    (values new-value
            (hash-table-count groups)
            (cond
              ((/= new-value 1) nil)
              (waitable?        (join/waitable))
              (t                (join))))))

(defmethod unref-group :around ((connection connection) (group string))
  (if (gethash group (connection-%groups connection))
      (call-next-method)
      (log:warn "~@<~A was asked to unreference unknown group ~A~@:>"
                connection group)))

(defmethod unref-group ((connection connection) (group string))
  (let+ (((&structure-r/o connection- %connection (groups %groups))
          connection)
         (new-value (decf (gethash group groups 0))))
    (when (zerop new-value)
      (log:info "~@<~A is leaving group ~A~@:>" connection group)
      (remhash group groups)
      (network.spread:leave %connection group))
    (values new-value (hash-table-count groups))))

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
    (let+ (((&structure connection- %connection %receive-buffer) connection)
           (buffer (or %receive-buffer
                       (setf %receive-buffer
                             (make-octet-vector
                              network.spread:+maximum-message-data-length+)))))
      (with-spread-condition-translation
        (values buffer
                (network.spread:receive-into %connection buffer
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
