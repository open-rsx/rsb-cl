;;; participant.lisp ---
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
  "DOC"
  (puri:merge-uris
   (make-instance 'puri:uri
		  :fragment (prin1-to-string
			     (participant-id participant)))
   (relative-url (participant-scope participant))))

(defmethod print-object ((object participant) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A" (scope-string (participant-scope object)))))


;;;
;;

;; TODO where should these go?

(defun uri-options (uri)
  "Translate the query part of URI into a plist of options."
  (let ((names-and-values (split-sequence #\= (puri:uri-query uri))))
    (iter (for name  in names-and-values       :by #'cddr)
	  (for value in (cdr names-and-values) :by #'cddr)
	  (collect (make-keyword (string-upcase name)))
	  (collect (parse-typed-value value)))))

(defun uri->scope-and-options (uri &optional defaults)
  "Dissect the URI as follows:
+ scheme   -> transport name
+ host     -> transport option :HOST
+ port     -> transport option :PORT
+ path     -> scope
+ fragment -> not processed
+ query    -> \"freestyle\" transport options
Return two values: scope and transport options."
  (bind (((:accessors-r/o
	   (transport   puri:uri-scheme)
	   (host        puri:uri-host)
	   (port        puri:uri-port)
	   (path        puri:uri-path)
	   (fragment    puri:uri-fragment)
	   (uri-options uri-options)) uri)
	 (options (copy-list
		   (rest (find transport defaults :key #'first)))))
    (when (eq transport :rsb)
      (error "~@<~S schema is not supported yet.~@:>"
	     transport))
    (when host
      (setf (getf options :host) host))
    (when port
      (setf (getf options :port) port))
    (when fragment
      (warn "~@<Ignoring fragment ~S in URI -> scope and options translation. URI was ~S~@:>"
	    fragment uri))
    (values (make-scope path)
	    (list (cons transport (append uri-options options))))))

;; TODO make a function uri->configuration?
(defmacro define-participant-creation-uri-methods (kind &rest args)
  (let* ((make-name      (symbolicate "MAKE-" kind))
	 (arg-names      (map 'list (compose #'first #'ensure-list)
			      args))
	 (designator-arg (first arg-names)))
    ;; We want the generated method to be specialized on URI
    ;; designators.
    (unless (eq (second (first args)) 'puri:uri)
      (error "~@<The specializer of the first parameter is ~S but should be ~S.~@:>"
	     (second (first args)) 'puri:uri))

    `(progn
       (defmethod ,make-name (,@args
			      &key
			      (transports (transport-options)))
	 (bind (((:values scope options)
		 (uri->scope-and-options ,designator-arg transports)))
	   (,make-name scope ,@(rest arg-names) :transports options )))

       (defmethod ,make-name ((,designator-arg string) ,@(rest args)
			      &key
			      (transports (transport-options)))
	 (if (find #\: ,designator-arg)
	     (,make-name (puri:parse-uri ,designator-arg) ,@(rest arg-names)
			 :transports transports)
	     (,make-name (make-scope ,designator-arg) ,@(rest arg-names)
			 :transports transports))))))

(defmacro define-participant-creation-restart-method (kind &rest args)
  "TODO(jmoringe): document"
  (let* ((make-name       (symbolicate "MAKE-" kind))
	 (arg-names       (map 'list (compose #'first #'ensure-list)
			       args))
	 (designator-arg  (first arg-names))
	 (designator-kind (symbolicate (second (first args))))
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
						    designator-kind kind)
				    ,designator-arg))
		  (go retry))
		(,restart-name (new-value)
		  :interactive (lambda ()
				 (format *query-io* ,(format nil "Specify ~A: " designator-kind))
				 (force-output *query-io*)
				 (list (read-line *query-io*)))
		  :report ,(format nil "Retry creating the ~(~A~) with a different ~(~A~)."
				   kind designator-kind)
		  (setf ,designator-arg new-value)
		  (go retry))))
	   result)))))
