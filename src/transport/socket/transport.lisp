;;;; transport.lisp --- Socket transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass socket-transport (transport)
  ((address-family :allocation :class
                   :reader   transport-address-family)
   (servers        :reader   transport-servers
                   :initform (make-hash-table :test #'equalp)
                   :documentation
                   "Map addresses to `bus-servers' instances.")
   (clients        :reader   transport-clients
                   :initform (make-hash-table :test #'equalp)
                   :documentation
                   "Map addresses to `bus-client' instances.")
   (lock           :reader   transport-lock
                   :initform (bt:make-recursive-lock "Bus clients+servers lock")
                   :documentation
                   "Protects accesses to clients and servers slots."))
  (:documentation
   "Superclass for socket-based transport classes."))

(defmethod print-items:print-items append ((object socket-transport))
  `((:client-count ,(hash-table-count (transport-clients object)) " (C ~D)"
                   ((:after :remote?)))
    (:server-count ,(hash-table-count (transport-servers object)) " (S ~D)"
                   ((:after :client-count)))))

;;; Ensuring bus clients and servers
;;;
;;; Depending on the ROLE parameter, find a bus provider ADDRESS and
;;; add CONNECTOR to it. If ROLE is :AUTO, first try to create a
;;; server provider and try to fall back to a client provider if that
;;; fails.

;;; Act as client/server unconditionally.

(macrolet
    ((define-ensure-bus-method (role slot bus-class socket)
       `(defmethod transport-ensure-bus ((transport socket-transport)
                                         (role      (eql ,role))
                                         (connector t)
                                         (address   t)
                                         &rest options
                                         ,@(when (eq role :server!)
                                             '(&key (if-leftover-connections :wait))))
          (log:trace "~@<~A is trying to obtain ~A bus ~A at ~{~S ~S~^ ~
                     ~} for ~A~@:>"
                     transport role
                     (transport-address-family transport) address connector)
          (let+ ((options (remove-from-plist options :if-leftover-connections))
                 ((&structure-r/o transport- address-family ,slot lock)
                  transport)
                 ((&flet make-socket ()
                    (apply #'make-socket address-family ,socket
                           (append address options))))
                 (key (cons address-family address)))
            (bt:with-recursive-lock-held (lock)
              (or (when-let ((candidate (gethash key ,slot)))
                    (with-locked-bus (candidate)
                      (when (bus-connectors candidate)
                        (check-connection-options
                         transport (bus-options candidate) options)
                        (notify connector candidate :attached)
                        candidate)))
                  (let ((bus (make-instance
                              ',bus-class
                              :make-socket #'make-socket
                              :options     options
                              ,@(when (eq role :server!)
                                  `(:if-leftover-connections if-leftover-connections)))))
                    (notify connector bus :attached)
                    (setf (gethash key ,slot) bus))))))))

  (define-ensure-bus-method :server! servers bus-server :passive)
  (define-ensure-bus-method :client! clients bus-client :active))

;;; Try as client/server and flip in case of error

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (role      (eql :server))
                                 (connector t)
                                 (address   t)
                                 &rest options)
  (restart-case
      (apply #'transport-ensure-bus transport :server! connector address
             options)
    (retry-as-client ()
      :report (lambda (stream)
                (format stream "~@<Try connecting to the ~A bus at ~
                                ~{~S ~S~^ ~} as client.~@:>"
                        (transport-address-family transport) address))
      (apply #'transport-ensure-bus transport :client! connector address
             options))))

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (role      (eql :client))
                                 (connector t)
                                 (address   t)
                                 &rest options)
  (restart-case
      (apply #'transport-ensure-bus transport :client! connector address
             options)
    (retry-as-server ()
      :report (lambda (stream)
                (format stream "~@<Try to create a server for the ~A ~
                                bus at ~{~S ~S~^ ~}.~@:>"
                        (transport-address-family transport) address))
      (apply #'transport-ensure-bus transport :server! connector address
             options))))

;;; Automatically decide whether to act as bus server or client

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (role      (eql :auto))
                                 (connector t)
                                 (address   t)
                                 &rest options)
  ;; Try to create a server bus provider and fall back to connecting
  ;; via a client bus provider if the server bus provider could not be
  ;; created.
  (bt:with-recursive-lock-held ((transport-lock transport))
    (handler-case
        (apply #'transport-ensure-bus transport :server! connector address
               options)
      ((or usocket:address-in-use-error
           usocket:address-not-available-error
           #+sbcl sb-bsd-sockets:socket-error)
          (server-condition)
        (with-condition-translation
            (((error socket-bus-auto-connection-error
                     :var           client-condition
                     :cause-initarg nil)
              :format-control   "Failed to get socket-based bus as ~
                                 server:~
                                 ~&~<> ~@;~A~:>~
                                 ~&Failed to get socket-based bus as ~
                                 client:~
                                 ~&~<> ~@;~A~:>"
              :format-arguments (list (list server-condition)
                                      (list client-condition))))
          (apply #'transport-ensure-bus transport :client! connector address
                 options))))))
