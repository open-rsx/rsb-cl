;;; informer.lisp --- Informers put events onto a bus.
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

(defclass informer (participant
		    rsb.ep:client
		    rsb.ep:broadcast-processor)
  ((type :initarg  :type
	 :type     (or symbol list)
	 :accessor informer-type ;; TODO really writable?
	 :initform t
	 :documentation
	 ""))
  (:documentation
   "In the RSB version of the communication pattern (Publish-Subscribe,
1:m-Communication) formed by publishers and subscribers, publishers
broadcast RSB events for subscribers to receive. Each publisher has an
associated SCOPE and type and known about its subscribers."))

(defmethod send :before ((informer informer) (data event))
  (let ((type (informer-type informer)))
    (unless (subtypep (event-type data) type)
      (error 'invalid-event-type
	     :datum         data
	     :expected-type type))))

(defmethod send ((informer informer) (event event))
  (rsb.ep:handle informer event))

(defmethod send ((informer informer) (data t))
  (send informer (make-event/typed (participant-scope informer)
				   data
				   (informer-type informer))))
;; TODO type: (type-of data) ? or let event decide?

(defmethod print-object ((object informer) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A ~S"
	    (scope-string  (participant-scope object))
	    (informer-type object))))


;;; `informer' creation
;;

(defmethod make-informer ((scope t)
			  (type  t)
			  &key
			  (transports (transport-options)))
  (let* ((configurator (make-instance 'rsb.ep:out-route-configurator
				      :scope scope))
	 (connectors   (iter (for (name . args) in transports)
			     (collect
				 (apply (fdefinition (find-symbol "MAKE-CONNECTOR" :rsb.transport))
					name :out args))))
	 (informer     (make-instance 'informer
				      :scope        scope
				      :type         type
				      :configurator configurator)))

    (setf (rsb.ep:configurator-connectors configurator) connectors)
    ;; TODO not sure who should set this up
    (push (rsb.ep:configurator-processor configurator) (rsb.ep:handlers informer))
    informer))

(define-participant-creation-uri-methods informer
    (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
    (scope scope) (type t))
(define-participant-creation-restart-method informer
    (scope puri:uri) (type t))
