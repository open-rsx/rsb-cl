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
	 :reader   informer-type
	 :initform t
	 :documentation
	 ""))
  (:documentation
   "In the RSB version of the communication pattern (Publish-Subscribe,
1:m-Communication) formed by publishers and subscribers, publishers
broadcast RSB events for subscribers to receive. Each publisher has an
associated SCOPE and type and known about its subscribers."))

(defmethod send :before ((informer informer) (data event))
  (bind (((:accessors-r/o (scope participant-scope)
			  (type  informer-type)) informer))
    ;; Ensure that the type of DATA is a subtype of INFORMER's type
    (unless (subtypep (event-type data) type)
      (error 'invalid-event-type
	     :event         data
	     :datum         data
	     :expected-type type))

    ;; Ensure that the destination scope of DATA is identical to
    ;; INFORMER's scope.
    (unless (scope= (event-scope data) (participant-scope informer))
      (error 'invalid-event-scope
	     :event          data
	     :expected-scope scope))))

(defmethod send ((informer informer) (event event))
  ;; Set EVENT's origin to our id.
  (setf (event-origin event) (participant-id informer))
  ;; Send EVENT.
  (rsb.ep:handle informer event)
  ;; Return event to the client in case we created it on the fly.
  event)

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
  (bind (((:values informer configurator)
	  (make-participant 'informer scope :out transports
			    :type type)))
    ;; TODO not sure who should set this up
    (push (rsb.ep:configurator-processor configurator) (rsb.ep:handlers informer))
    informer))

(define-participant-creation-uri-methods informer
    (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
    (scope scope) (type t))
(define-participant-creation-restart-method informer
    (scope puri:uri) (type t))
