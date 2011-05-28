;;; protocol.lisp --- Protocols provided by the event-processing module.
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


;;; Push source protocol
;;
;; This protocol is provided by event sources that emit events without
;; requiring an external trigger, for example a thread polling a
;; network connection.

(defgeneric handlers (source)
  (:documentation
   "Return the handlers associated to SOURCE."))

(defgeneric (setf handlers) (new-value source)
  (:documentation
   "Set the list of handlers associated to SOURCE to NEW-VALUE."))


;;; Pull source protocol
;;
;; This protocol is provided by event sources that emit events only
;; after being triggered externally. This external triggering is
;; achieve by calling `emit' with such a source.

(defgeneric emit (source block?)
  (:documentation
   "Block until an event is available from SOURCE."))


;;; Sink protocol
;;

(defgeneric handle (sink data)
  (:documentation
   "Put DATA into SINK. SINK may for example process, relay or discard
DATA."))


;;; Default behavior
;;

(defmethod handle ((sink function)
		   (data t))
  "If SINK is a function, call it with DATA."
  (funcall sink data))

(defmethod handle ((sink list)
		   (data t))
  "If SINK is a list, treat it as a list of sinks and let each
contained sink handle DATA."
  (map nil (rcurry #'handle data) sink))


;;; Dispatching processor protocol
;;

(defgeneric dispatch (processor event)
  (:documentation
   "Dispatch EVENT in the manner implemented by PROCESSOR. This may
mean to call some handlers, for example."))


;;; Notification protocol
;;

(defgeneric notify (recipient subject action)
  (:documentation
   "Methods should return one of the symbols :not-implemented
or :implemented to indicate whether the combination of SUBJECT and
ACTION could be implemented by RECIPIENT."))


;;; Processor classes
;;

(dynamic-classes:define-dynamic-class-family processor
    "This class family consists of dynamically constructed processor
classes. This facility can be used to construct appropriate processor
classes based on configuration information supplied at runtime.")
