;;;; pull-processor.lisp --- Pull-based processor implementation.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass pull-processor ()
  ((connectors    :initarg  :connectors
                  :type     list
                  :accessor processor-connectors
                  :initform '()
                  :documentation
                  "Stores a list of connector instances from which the
                   processor retrieves events.")
   (current-event :accessor processor-current-event
                  :initform nil
                  :documentation
                  "Stores the event currently being retrieved from a
                   connector."))
  (:documentation
   "Allows pull-based, blocking and non-blocking retrieval of events
    from multiple connectors.

    To achieve this, the processor is added to the handler list of
    each connector. When asked to retrieve an event, it walks through
    its list of connectors, asking each connector to emit an event
    until one succeeds. The emitted event is dispatched to the
    processor via the `handle' interface. This enables arbitrary event
    processing by methods on `handle' in the usual way."))

(defmethod notify ((processor pull-processor)
                   (connector t)
                   (action    (eql :connector-added)))
  (log:trace "~@<~A is storing added connector ~A~@:>" processor connector)
  (push connector (processor-connectors processor)))

(defmethod notify ((processor pull-processor)
                   (connector t)
                   (action    (eql :connector-removed)))
  (log:trace "~@<~A is deleting removed connector ~A~@:>" processor connector)
  (removef (processor-connectors processor) connector :count 1))

(defmethod emit :before ((processor pull-processor)
                         (block?    t))
  ;; Warn the caller that we cannot currently block on multiple
  ;; connectors properly.
  (when (and (> (length (processor-connectors processor)) 1)
             block?)
    (warn "~@<Requested blocking emit for multiple connectors; This ~
           does not work properly, yet.~@:>")))

(defmethod emit ((processor pull-processor)
                 (block?    t))
  ;; Ask connectors associated to PROCESSOR to emit an event. Return
  ;; the event or maybe nil when BLOCK? is nil.
  (let+ (((&structure processor- connectors current-event) processor))
    (setf current-event nil)
    ;; Round-robin for multiple connectors in non-blocking mode.
    (iter (some (rcurry #'emit block?)
                (setf connectors (rotate connectors -1)))
          (until (or current-event (not block?))))
    current-event))

(defmethod handle ((processor pull-processor)
                   (event     event))
  ;; Store EVENT in PROCESSOR so it can be returned from the
  ;; surrounding `emit' call.
  (setf (processor-current-event processor) event))
