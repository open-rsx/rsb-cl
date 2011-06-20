;;; pull-processor.lisp --- Pull-based processor implementation.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.event-processing)

(defclass pull-processor ()
  ((connectors    :initarg  :connectors
		  :type     list
		  :accessor processor-connectors
		  :initform nil
		  :documentation
		  "A list of connector instances from which the
processor retrieves events.")
   (current-event :initarg  :current-event
		  :type     t
		  :accessor processor-current-event
		  :initform nil
		  :documentation
		  "The event currently being retrieved from a
connector."))
  (:documentation
   "Instances of this class allow pull-based, blocking and
non-blocking retrieval of events from multiple connectors. To achieve
this, the processor is added to the handler list of each
connector. When asked to retrieve an event, it walks through its list
of connectors, asking each connector to emit an event until one
succeeds. The emitted event is dispatched to the processor via the
`handle' interface. This enables arbitrary event processing by methods
on `handle' in the usual way."))

(defmethod notify ((processor pull-processor)
		   (connector t)
		   (action    (eql :connector-added)))
  (log1 :info processor "Storing added connector ~S" connector)
  (push connector (processor-connectors processor)))

(defmethod notify ((processor pull-processor)
		   (connector t)
		   (action    (eql :connector-removed)))
  (log1 :info processor "Deleting removed connector ~S" connector)
  (removef (processor-connectors processor) connector :count 1))

(defmethod emit :before ((processor pull-processor)
			 (block?    t))
  "Warn the caller that we cannot currently block on multiple
connectors properly."
  (when (and (> (length (processor-connectors processor)) 1)
	     block?)
    (warn "~@<Requested blocking emit for multiple connectors; This ~
does not work properly, yet.~@:>")))

(defmethod emit ((processor pull-processor)
		 (block?    t))
  "Ask connectors associated to PROCESSOR to emit an event. Return the
event or maybe nil when BLOCK? is nil."
  (bind (((:accessors (connectors    processor-connectors)
		      (current-event processor-current-event)) processor))
    (setf current-event nil)
    ;; Round-robin for multiple connectors in non-blocking mode.
    (iter (some (rcurry #'emit block?)
		(setf connectors (rotate connectors -1)))
	  (until (or current-event (not block?))))
    current-event))

(defmethod handle ((processor pull-processor)
		   (event     event))
  "Store EVENT in PROCESSOR so it can be returned from the surrounding
`emit' call."
  (setf (processor-current-event processor) event))
