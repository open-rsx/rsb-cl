;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
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

(in-package :rsb)

(defclass listener (participant
		    rsb.ep:client)
  ((enabled? :type     boolean
	     :accessor listener-enabled?
	     :initform t
	     :documentation
	     "This Boolean value controls whether the listener
participates in the filtering and dispatching of events."))
  (:documentation
   "A listener consists of a set of filters, a set of handlers and has
a mechanism for dispatching matching events to these handlers."))

(defmethod listener-filters ((listener listener))
  "DOC"
  (rsb.ep:configurator-filters (rsb.ep:client-configurator listener)))

(defmethod (setf listener-filters) ((new-value list)
				    (listener  listener))
  "DOC"
  (setf (rsb.ep:configurator-filters
	 (rsb.ep:client-configurator listener))
	new-value))

(defmethod rsb.ep:handlers ((listener listener))
  "DOC"
  ;(rsb.ep:handlers (rsb.ep:client-configurator listener))
  nil)

(defmethod (setf rsb.ep:handlers) ((new-value list) (listener listener))
  "DOC"
  (rsb.ep:notify (rsb.ep:client-configurator listener) (first new-value) :handler-added))
  ;(setf (rsb.ep:handlers (rsb.ep:client-configurator listener))
;	new-value))

(defmethod (setf listener-enabled?) :around ((new-value    t)
					     (listener listener))
  "DOC"
  (let ((old-value (listener-enabled? listener)))
    (prog1
	(call-next-method)
      (unless (eq old-value new-value)
	(rsb.ep:notify (rsb.ep:client-configurator listener)
		       listener
		       (if new-value :enabled :disabled))))))

(defmethod print-object ((object listener) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)~:[ [disabled]~;~]"
	    (scope-string (participant-scope object))
	    0 (listener-enabled? object))))


;;; `listener' creation
;;

;; TODO make-informer and make-reader are very similar
(defmethod make-listener ((scope scope)
			  &key
			  (transports (transport-options)))
  (let* ((configurator (make-instance 'rsb.ep:in-route-configurator
				      :scope scope))
	 (connectors   (funcall (fdefinition (find-symbol "MAKE-CONNECTORS" :rsb.transport)) ;; TODO package deps
				transports :in-push))
	 (listener     (make-instance 'listener
				      :scope        scope
				      :configurator configurator)))

    (setf (rsb.ep:configurator-connectors configurator) connectors)

    listener))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
