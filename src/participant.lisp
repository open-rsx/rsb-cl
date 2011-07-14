;;; participant.lisp --- A superclass for participant classes.
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

(defclass participant (uuid-mixin
		       scope-mixin)
  ((id    :reader participant-id)
   (scope :reader participant-scope))
  (:documentation
   "Instances of this class participate in the exchange of
notifications on one channel of the bus."))

(defmethod relative-url ((participant participant))
  (puri:merge-uris
   (make-instance 'puri:uri
		  :fragment (prin1-to-string
			     (participant-id participant)))
   (relative-url (participant-scope participant))))

(defmethod detach ((participant participant))
  "Let PARTICIPANT's configurator do the heavy lifting."
  (detach (rsb.ep:client-configurator participant)))

(defmethod print-object ((object participant) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A" (scope-string (participant-scope object)))))


;;; Participant creation
;;

;;; TODO(jmoringe): infer direction?
(defun make-participant (class scope direction transports &rest args)
  "Make and return a participant instance of CLASS that participates
in the channel designated by SCOPE. DIRECTION is one
of :in-push, :in-pull and :out. TRANSPORTS is a list of connector
classes. ARGS are arguments for the created transport instances.

Return three values:
+ the `participant' instance
+ the associated `rsb.event-processing:configurator' instance
+ the list of instantiated `rsb.transport:connectors'"
  ;; Signal an error if no transports have been supplied.
  (unless transports
    (error 'no-transports
	   :scope scope))

  (let* ((configurator (make-instance
			(ecase direction
			  ((:in-push :in-pull) 'rsb.ep:in-route-configurator)
			  (:out                'rsb.ep:out-route-configurator))
			:scope     scope
			:direction direction))
	 (connectors   (funcall (fdefinition (find-symbol "MAKE-CONNECTORS" :rsb.transport)) ;; TODO figure out package deps
				transports direction))
	 (participant  (apply #'make-instance class
			      :scope        scope
			      :configurator configurator
			      args)))

    ;; Associate constructed CONNECTORS to CONFIGURATOR instance.
    (setf (rsb.ep:configurator-connectors configurator) connectors)

    (values participant configurator connectors)))

(defmacro define-participant-creation-uri-methods (kind &rest args)
  (let* ((make-name      (symbolicate "MAKE-" kind))
	 (arg-names      (map 'list (compose #'first #'ensure-list)
			      args))
	 (designator-arg (first arg-names)))
    ;; We want the generated method to be specialized on URI
    ;; designators.
    (unless (eq (second (first args)) 'puri:uri)
      (error "~@<The specializer of the first parameter is ~S, but ~
should be ~S.~@:>"
	     (second (first args)) 'puri:uri))

    `(progn
       ;; This method operates on URIs.
       (defmethod ,make-name (,@args
			      &key
			      (transports (transport-options)))
	 (bind (((:values scope options)
		 (uri->scope-and-options ,designator-arg transports)))
	   (,make-name scope ,@(rest arg-names) :transports options)))

       ;; This method operates on strings which it turns into either
       ;; URIs (if the string contains a colon) or scopes.
       (defmethod ,make-name ((,designator-arg string) ,@(rest args)
			      &key
			      (transports (transport-options)))
	 (if (find #\: ,designator-arg)
	     (,make-name (puri:parse-uri ,designator-arg) ,@(rest arg-names)
			 :transports transports)
	     (,make-name (make-scope ,designator-arg) ,@(rest arg-names)
			 :transports transports))))))

(defmacro define-participant-creation-restart-method (kind &rest args)
  "Emit an :around method on `make-KIND' that establishes restarts.
KIND will usually be one of :informer, :listener and :reader. ARGS is
a method lambda-list. The first argument is assumed to be designator
that is the URI or scope."
  (let* ((make-name       (symbolicate "MAKE-" kind))
	 (arg-names       (map 'list (compose #'first #'ensure-list)
			       args))
	 (designator-arg  (first arg-names))
	 (designator-kind (make-keyword (second (first args))))
	 (restart-name    (symbolicate "USE-" designator-kind)))
    (with-unique-names (args-var)
      `(defmethod ,make-name :around (,@args
				      &rest ,args-var
				      &key &allow-other-keys)
	 "Install restarts around the creation attempt."
	 (let (result)
	   (tagbody
	    retry
	      (restart-case
		  (setf result (apply #'call-next-method ,@arg-names ,args-var))
		(retry ()
		  :report (lambda (stream)
			    (format stream ,(format nil "~~@<Retry creating the ~(~A~) for ~(~A~) ~~S~~@:>"
						    kind designator-kind)
				    ,designator-arg))
		  (go retry))
		(,restart-name (new-value)
		  :interactive (lambda ()
				 (format *query-io* ,(format nil "Specify ~(~A~) (not evaluated): "
							     designator-kind))
				 (force-output *query-io*)
				 (list ,(ecase designator-kind
				          (:uri   `(puri:parse-uri (read-line *query-io*)))
					  (:scope `(make-scope (read-line *query-io*))))))
		  :report ,(format nil "Retry creating the ~(~A~) with a different ~(~A~)."
				   kind designator-kind)
		  (setf ,designator-arg new-value)
		  (go retry))))
	   result)))))
