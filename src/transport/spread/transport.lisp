;;;; transport.lisp --- Spread transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `spread-transport' transport class

(defclass spread-transport (transport)
  ((buses :reader   %buses
          :initform (make-hash-table :test #'equalp)
          :documentation
          "Maps Spread connection options to `bus' instances.")
   (lock  :reader   %lock
          :initform (bt:make-recursive-lock "Connections lock")
          :documentation
          "Protects the bus map."))
  (:documentation
   "Stores bus instances."))

;;; Transport registration

(register-transport
 :spread
 :transport-class 'spread-transport
 :schemas         :spread
 :wire-type       'nibbles:octet-vector
 :remote?         t
 :documentation
 "A transport using the Spread group communication framework.

  This transport maps scopes to Spread groups to allow multicast-based
  distribution of events to interested processes.")

(defmethod print-items:print-items append ((object spread-transport))
  (let ((bus-count (hash-table-count (%buses object))))
    `((:bus-count ,bus-count " (B ~D)" ((:after :remote?))))))

(defmethod ensure-access ((transport spread-transport)
                          (options   t)
                          (connector t))
  (let+ (((&accessors-r/o (buses %buses) (lock %lock)) transport)
         ((&plist-r/o (name       :name)
                      (tcpnodelay :tcpnodelay)
                      (age-limit  :age-limit))
          options)
         (key (list name tcpnodelay age-limit)))
    (log:debug "~@<~A is obtaining a bus for options ~S~@:>" transport key)
    (bt:with-lock-held (lock)
      (or (when-let ((candidate (gethash key buses)))
            (bt:with-recursive-lock-held ((%connectors-lock candidate))
              (when (%connectors candidate)
                (notify connector candidate :attached)
                candidate)))
          (let* ((connection (make-instance 'connection :name name))
                 (bus        (make-instance 'bus
                                            :connection connection
                                            :age-limit  age-limit)))
            (notify connector bus :attached)
            (setf (gethash key buses) bus))))))
