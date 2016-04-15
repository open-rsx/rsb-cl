;;;; connection.lisp --- Spread connections with membership management.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `connection' class

(defclass connection (print-items:print-items-mixin)
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
    (missing-required-initarg 'connection :either-name-or-connection)))

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

(defmethod connection-name ((connection connection))
  (network.spread:connection-name (connection-%connection connection)))

(defmethod connection-daemon-name ((connection connection))
  (network.spread:connection-daemon-name (connection-%connection connection)))

(defmethod connection-groups ((connection connection))
  (hash-table-keys (connection-%groups connection)))

(defmethod detach ((connection connection))
  (network.spread:disconnect (connection-%connection connection)))

(let+ (((&flet make-hook-promise (context connection hook group predicate)
          (let+ ((hook    (hooks:object-hook connection hook))
                 (promise (lparallel:promise))
                 ((&labels notify-and-remove (message-group members)
                    (log:debug "~@<~A got ~A notification for ~
                                group ~S with members ~S.~@:>"
                               connection context message-group members)
                    (when (and (string= message-group group :end2 31)
                               (funcall predicate members))
                      (hooks:remove-from-hook hook #'notify-and-remove)
                      (lparallel:fulfill promise)))))
            (hooks:add-to-hook hook #'notify-and-remove)
            promise))))

  (defmethod ref-group ((connection connection) (group string)
                        &key (waitable? nil))
    (let+ (((&structure-r/o connection- %connection (groups %groups))
            connection)
           ((&flet join ()
              (log:info "~@<~A is joining group ~A~@:>" connection group)
              (network.spread:join %connection group)
              t))
           ((&flet join/waitable ()
              (let ((name (network.spread:connection-name %connection)))
                (prog1
                    (make-hook-promise
                     "join" %connection 'network.spread:join-hook group
                     (lambda (members)
                       (member name members :test #'string=)))
                  (join)))))
           (new-value (incf (gethash group groups 0))))
      (values new-value
              (hash-table-count groups)
              (cond
                ((/= new-value 1) nil)
                (waitable?        (join/waitable))
                (t                (join))))))

  (defmethod unref-group :around ((connection connection) (group string)
                                  &key waitable?)
    (declare (ignore waitable?))
    (if (gethash group (connection-%groups connection))
        (call-next-method)
        (log:warn "~@<~A was asked to unreference unknown group ~A~@:>"
                  connection group)))

  (defmethod unref-group ((connection connection) (group string)
                          &key (waitable? nil))
    (let+ (((&structure-r/o connection- %connection (groups %groups))
            connection)
           ((&flet leave ()
              (log:info "~@<~A is leaving group ~A~@:>" connection group)
              (remhash group groups)
              (network.spread:leave %connection group)
              t))
           ((&flet leave/waitable ()
              (let ((name (network.spread:connection-name %connection)))
                (prog1
                    (make-hook-promise
                     "leave" %connection 'network.spread:leave-hook group
                     (lambda (members)
                       (not (member name members :test #'string=))))
                  (leave)))))
           (new-ref-count   (decf (gethash group groups 0)))
           (maybe-promise   (cond
                              ((plusp new-ref-count) nil)
                              (waitable?             (leave/waitable))
                              (t                     (leave))))
           (new-group-count (hash-table-count groups)))
      (values new-ref-count new-group-count maybe-promise))))

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
      ;; Use :when-membership for efficiency: we need join and leave
      ;; hooks to run with sender and group information, but regular
      ;; messages should be fast and do not need sender and group
      ;; information.
      (with-spread-condition-translation
        (when-let ((length (network.spread:receive-into
                            %connection buffer
                            :block?         block?
                            :return-sender? :when-membership
                            :return-groups? :when-membership)))
          (make-wire-notification buffer length)))))

  (defmethod send-message ((connection  connection)
                           (destination list)
                           (payload     wire-notification))
    (let+ (((&structure-r/o wire-notification- buffer end) payload))
      (assert (= (length buffer) end))
      (with-spread-condition-translation
        (network.spread:send-bytes
         (connection-%connection connection) destination buffer)))))

(defmethod print-items:print-items append ((object connection))
  (let+ (((&structure-r/o connection- name daemon-name %groups) object))
    `((:name        ,name                       "~A")
      (:daemon-name ,daemon-name                "@~A"   ((:after :name)))
      (:group-count ,(hash-table-count %groups) " (~D)" ((:after :daemon-name))))))
