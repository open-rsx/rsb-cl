;;; informer.lisp --- Informers put events onto a bus.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb)

(defclass informer (participant
		    rsb.ep:client
		    rsb.ep:broadcast-processor)
  ((type                      :initarg  :type
			      :type     (or symbol list)
			      :reader   informer-type
			      :initform t
			      :documentation
			      "")
   (sequence-number-generator :type     function
			      :reader   %informer-sequence-number-generator
			      :initform (make-sequence-number-generator)
			      :documentation
			      "Stores a thread-safe sequence number
generation function which is used to generate sequence numbers for
events sent by this informer."))
  (:documentation
   "An informer is a participant that publishes events to a specific
channel. Other participants on the same channel or a channel that
includes the informer's channel can receive these events. It is
possible for multiple informers to send events for the same
channel."))

(defmethod send :before ((informer informer) (data event)
			 &key
			 unchecked?)
  (when unchecked?
    (return-from send))

  (let+ (((&accessors-r/o (scope participant-scope)
			  (type  informer-type)) informer))
    ;; Ensure that the type of DATA is a subtype of INFORMER's type.
    (unless (typep (event-data data) type)
      (error 'invalid-event-type
	     :event         data
	     :datum         data
	     :expected-type type))

    ;; Ensure that the destination scope of DATA is identical to
    ;; INFORMER's scope.
    (unless (sub-scope? (event-scope data) (participant-scope informer))
      (error 'invalid-event-scope
	     :event          data
	     :expected-scope scope))))

(defmethod send ((informer informer) (event event)
		 &rest meta-data
		 &key
		 method
		 causes)
  ;; Set EVENT's sequence number to our next sequence number and
  ;; origin to our id.
  (setf (event-sequence-number event)
	(funcall (%informer-sequence-number-generator informer))
	(event-origin event)
	(participant-id informer))

  (when method
    (setf (event-method event) method))

  ;; Additional meta-data.
  (iter (for (key value) on meta-data :by #'cddr)
	(unless (member key '(:method :causes :unchecked?))
	  (setf (meta-data event key) value)))

  ;; Additional causes.
  (when causes
    (appendf (event-causes event) causes))

  ;; Send EVENT.
  (rsb.ep:handle informer event)

  ;; Return EVENT to the client in case we created it on the fly.
  event)

(defmethod send ((informer informer) (data t)
		 &rest args
		 &key)
  (apply #'send informer
	 (make-event (participant-scope informer) data)
	 args))

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
			  (transports (transport-options))
			  (converters (default-converters)))
  (handler-bind
      ;; Translate different kinds of errors into
      ;; `informer-creation-failed' errors.
      ((error #'(lambda (condition)
		  (error 'informer-creation-failed
			 :scope      scope
			 :transports transports
			 :type       type
			 :cause      condition))))
    (let+ (((&values informer configurator)
	    (make-participant 'informer scope :out transports converters
			      :type type)))
      ;; Connect the processor of CONFIGURATOR to INFORMER as an event
      ;; handler.
      (push (rsb.ep:configurator-processor configurator)
	    (rsb.ep:handlers informer))

      ;; Return the ready-to-use `informer' instance.
      informer)))

(define-participant-creation-uri-methods informer
    (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
    (scope scope) (type t))
(define-participant-creation-restart-method informer
    (scope puri:uri) (type t))
