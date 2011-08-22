;;; server.lisp --- A superclass for local and remote server classes.
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

(in-package :rsb.patterns)


;;; `method1' class
;;

(defclass method1 ()
  ((server   :initarg  :server
	     :type     server
	     :reader   method-server
	     :writer   (setf %method-server)
	     :documentation
	     "Stores the server object to which the method belongs.")
   (name     :initarg  :name
	     :type     string
	     :reader   method-name
	     :documentation
	     "Stores the name of the method.")
   (informer :initarg  :informer
	     :type     (or null informer)
	     :reader   method-informer
	     :writer   (setf %method-informer)
	     :initform nil
	     :documentation
	     "Stores the `informer' instance associated to the
method. The instance is created lazily when first used.")
   (listener :initarg  :listener
	     :type     (or null listener)
	     :reader   method-listener
	     :writer   (setf %method-listener)
	     :initform nil
	     :documentation
	     "Stores the `listener' instance associated to the
method. The instance is created lazily when first used."))
  (:documentation
   "This class serves as a superclass for local and remote method
classes."))

(defmethod detach ((method method1))
  (bind (((:accessors-r/o (informer method-informer)
			  (listener method-listener)) method))
    ;; For the sake of the `local-method' subclass: shutdown the
    ;; listener first. This will prevent new method calls from being
    ;; initiated and wait for in-progress calls to finish while still
    ;; having the informer available for sending out reply events.
    ;;
    ;; This shutdown sequence doesn't help `remote-method', but the
    ;; client should not detach a `remote-server' with in-progress
    ;; method calls and expect replies to arrive anyway.
    (when listener (detach listener))
    (when informer (detach informer))))

(defmethod print-object ((object method1) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (method-name object))))

(defmacro define-lazy-creation-method (class slot args scope)
  "Define a :before method on the reader function for SLOT of CLASS
that creates the slot value, if required. ARGS are passed to the
creation function and SCOPE is used to compute the scope of the
created participant."
  (let ((method-name (symbolicate "METHOD-" slot))
	(writer-name (symbolicate "%METHOD-" slot))
	(make-name   (symbolicate "MAKE-" slot)))
    `(defmethod ,method-name :before ((method ,class))
       ,(format nil "Lazily create the ~(~A~) when it is first requested."
		slot)
       (unless (slot-value method ',slot)
	 (bind (((:accessors-r/o (server method-server)
				 (name   method-name)) method))
	   (setf (,writer-name method)
		 (,make-name (%make-scope server ,scope name) ,@args
			     :transports (server-transport-options server))))))))
;;; TODO(jmoringe): override configured error policy


;;; `server' class
;;

(defclass server (participant)
  ((transport-options :initarg  :transport-options
		      :type     list
		      :reader   server-transport-options
		      :documentation
		      "Stores the transport options that should be
used by participants which implement the actual communication on
behalf of the server.")
   (methods           :initarg  :methods
		      :type     hash-table
		      :reader   %server-methods
		      :initform (make-hash-table :test #'equal)
		      :documentation
		      "Stores a mapping of method names to method
objects."))
  (:documentation
   "This class serves as a superclass for local and remote server
classes. It provides storage of transport options and methods and
generic support for retrieving, adding and removing methods."))

(defmethod server-methods ((server server))
  (hash-table-values (%server-methods server)))

(defmethod server-method ((server server)
			  (name   string)
			  &key
			  (error? t))
  (let ((method (gethash name (%server-methods server))))
    (or method
	(when error?
	  (error 'no-such-method
		 :name name)))))

(defmethod (setf server-method) ((new-value method1)
				 (server    server)
				 (name      string))
  (setf (%method-server new-value)              server
	(gethash name (%server-methods server)) new-value))

(defmethod (setf server-method) ((new-value (eql nil))
				 (server    server)
				 (name      string))
  (detach (server-method server name))
  (remhash name (%server-methods server))
  new-value)

(defmethod detach ((server server))
  (iter (for method in (server-methods server))
	(restart-case
	    ;; Give each method ten seconds to detach. If one takes
	    ;; longer, allow skipping it.
	    #+sbcl (sb-ext:with-timeout 10
		     (setf (server-method server (method-name method)) nil))
	    #-sbcl (detach method)
	    (continue ()
	      :report "~@<Ignore the error and continue with the ~
remaining methods.~@:>"))))

(defmethod print-object ((object server) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A (~D)"
	    (scope-string (participant-scope object))
	    (hash-table-count (%server-methods object)))))


;;; Utility functions
;;

(defun %make-scope (participant &rest components)
  "Return a scope that extends the scope of PARTICIPANT with
COMPONENTS."
  (merge-scopes (format nil "~{/~A~}" components)
		(participant-scope participant)))
