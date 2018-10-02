;;;; out-connector.lisp --- Out-direction connector for Spread transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass out-connector (restart-notification-sender-mixin
                         error-handling-sender-mixin
                         timestamping-sender-mixin
                         connector)
  ((max-fragment-size   :initarg  :max-fragment-size
                        :type     positive-fixnum
                        :reader   connector-max-fragment-size
                        :initform 100000
                        :documentation
                        #.(format nil "The maximum payload size that ~
                           may be send in a single notification. The ~
                           value of this options has to be chosen such ~
                           that the combined sizes of payload and ~
                           envelope data of notifications remain below ~
                           the maximum message size allowed by ~
                           Spread."))
   (scope->groups-cache :reader   connector-%scope->groups-cache
                        :initform (make-scope->groups-cache)))
  (:metaclass connector-class)
  (:direction :out)
  (:options
   (:max-fragment-size &slot))
  (:documentation
   "A connector for sending data over Spread."))

(register-connector :spread :out 'out-connector)

(defmethod notify ((recipient out-connector)
                   (subject   scope)
                   (action    t))
  (notify recipient t action))

(defmethod handle ((sink out-connector) (data event))
  (let* ((notification  (event->notification sink data))
         (notifications (event->notification sink notification)))
    (declare (type function notifications))
    (loop :for notification = (funcall notifications)
          :while notification
          :do (send-notification sink notification))))

(defmethod event->notification ((connector out-connector) (event event))
  (let+ (((&structure-r/o connector- converter (cache %scope->groups-cache))
          connector)
         ((&structure-r/o
           event- origin sequence-number scope method timestamps causes data)
          event)
         (meta-data (rsb:event-meta-data event))
         (group-names (scope->groups scope cache))
         ((&values wire-data wire-schema)
          (rsb.converter:domain->wire converter data))
         (notification (make-notification
                        sequence-number origin scope method
                        wire-schema meta-data timestamps causes)))
    (make-outgoing-notification scope group-names notification wire-data)))

(defmethod event->notification ((connector out-connector)
                                (event     outgoing-notification))
  (let+ (((&structure-r/o connector- max-fragment-size) connector)
         ((&structure-r/o
           outgoing-notification- destination notification wire-data)
          event)
         (splitter (split-notification notification wire-data max-fragment-size)))
    (lambda ()
      (let+ (((&values fragment part num-parts) (funcall splitter)))
        (when fragment
          (let* ((buffer       (pb:pack* fragment))
                 (notification (make-destined-wire-notification
                                destination buffer (length buffer))))
            (declare (type simple-octet-vector buffer))
            (values notification part num-parts)))))))

(defmethod send-notification ((connector    out-connector)
                              (notification destined-wire-notification))
  ;; Send NOTIFICATION using `send-message'. The primary purpose of
  ;; this method is sending the notifications with restarts installed.
  (let+ (((&accessors-r/o (connection connector-connection)) connector)
         ((&structure-r/o destined-wire-notification- destination) notification))
    (send-message connection destination notification)))
