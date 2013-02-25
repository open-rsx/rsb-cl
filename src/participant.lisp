;;; participant.lisp --- A superclass for participant classes.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(defclass participant (uuid-mixin
		       scope-mixin)
  ((id         :reader   participant-id)
   (scope      :reader   participant-scope)
   (converters :initarg  :converters
	       :type     list
	       :initform nil
	       :reader   participant-converters
	       :documentation
	       "Stores a list of the converters available for use in
connectors of the participant. Each element is of the form

  (WIRE-TYPE . CONVERTER)

.")
   (transform  :initarg  :transform
	       :reader   participant-transform
	       :initform nil
	       :documentation
	       "Stores the transform which should be applied to
processed events.")
   (error-hook :type     list
	       :initform nil
	       :documentation
	       "Stores a list of functions to call in case of
errors."))
  (:documentation
   "Instances of this class participate in the exchange of
notifications on one channel of the bus."))

(defmethod participant-converter ((participant participant)
				  (wire-type   t)
				  &key &allow-other-keys)
  "Return the converter for WIRE-TYPE that is used by the connectors
of PARTICIPANT."
  (mapcar #'cdr
	  (remove wire-type (participant-converters participant)
		  :key      #'car
		  :test-not #'subtypep)))

(defmethod participant-error-hook ((participant participant))
  (hooks:object-hook participant 'error-hook))

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

(defun make-participant (class scope direction
			 transports converters transform
			 &rest args)
  "Make and return a participant instance of CLASS that participates
in the channel designated by SCOPE.

DIRECTION is one of :in-push, :in-pull and :out.

TRANSPORTS is a list of connector classes.

CONVERTERS is an alist of converters for particular wire-types with
items of the form (WIRE-TYPE . CONVERTER).

When non-nil, TRANSFORM is a transform object usable with
`rsb.event-processing:transform!'.

ARGS are arguments for the created CLASS instance.

Return three values:
+ the `participant' instance
+ the associated `rsb.event-processing:configurator' instance
+ the list of instantiated `rsb.transport:connectors'"
  ;; Signal an error if no transports have been supplied.
  (unless transports
    (error 'no-transports
	   :scope scope))

  ;; Replace &inherit marker in transport options with actual default
  ;; options for respective transports.
  (setf transports (map 'list #'process-transport-options transports))

  ;; Ensure that CONVERTERS is an alist of items of the form
  ;; (WIRE-TYPE . CONVERTER).
  (unless (and (listp converters) (consp (first converters)))
    (setf converters (list (cons t converters))))

  (let* ((configurator (make-instance
			(ecase direction
			  ((:in-push :in-pull) 'rsb.ep:in-route-configurator)
			  (:out                'rsb.ep:out-route-configurator))
			:scope     scope
			:direction direction
			:transform transform))
	 (connectors   (rsb.transport:make-connectors
			transports direction converters))
	 (participant  (apply #'make-instance class
			      :scope        scope
			      :converters   converters
			      :configurator configurator
			      args)))

    ;; Associate constructed CONNECTORS to CONFIGURATOR instance.
    (setf (rsb.ep:configurator-connectors configurator) connectors)

    ;; Setup the error hook of PARTICIPANT to be run for all errors
    ;; intercepted by CONFIGURATOR.
    (setf (rsb.ep:processor-error-policy configurator)
	  #'(lambda (condition)
	      (hooks:run-hook
	       (hooks:object-hook participant 'error-hook) condition)
	       ;;; TODO(jmoringe): maybe (ignore-error) here?
	      ))

    (values participant configurator connectors)))

(defmacro define-participant-creation-uri-methods (kind &rest args)
  (let* ((make-name      (symbolicate "MAKE-" kind))
	 (arg-names      (mapcar (compose #'first #'ensure-list) args))
	 (designator-arg (first arg-names)))
    ;; We want the generated method to be specialized on URI
    ;; designators.
    (unless (eq (second (first args)) 'puri:uri)
      (error "~@<The specializer of the first parameter is ~S, but ~
should be ~S.~@:>"
	     (second (first args)) 'puri:uri))

    `(progn
       ;; This method operates on URIs.
       (defmethod ,make-name
	   (,@args
	    &key
	    (transports (transport-options
			 :exclude-disabled?
			 (not (uri-transport ,designator-arg))))
	    (converters (default-converters))
	    transform)
	 (let+ (((&values scope options)
		 (uri->scope-and-options ,designator-arg transports)))
	   (,make-name scope ,@(rest arg-names)
		       :transports options
		       :converters converters
		       :transform  transform)))

       ;; This method operates on strings which it turns into either
       ;; URIs (if the string contains a colon) or scopes.
       (defmethod ,make-name ((,designator-arg string) ,@(rest args)
			      &key
			      (transports nil transports-supplied?)
			      (converters nil converters-supplied?)
			      transform)
	 (apply #',make-name
		(if (find #\: ,designator-arg)
		    (puri:parse-uri ,designator-arg)
		    (make-scope ,designator-arg))
		,@(rest arg-names)
		:transform transform
		(append
		 (when transports-supplied?
		   (list :transports transports))
		 (when converters-supplied?
		   (list :converters converters))))))))

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
