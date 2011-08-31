;;; util.lisp ---
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


;;; Sequence number functions
;;

(declaim (ftype (function (&optional sequence-number)
			  (function () sequence-number))
		make-sequence-number-generator))

(defun make-sequence-number-generator (&optional (start 0))
  "Return a function that returns increasing numbers starting with
START."
  #+sbcl
  (let ((current (list (list start))))
    (declare (type (cons (cons sequence-number null) null) current))
    #'(lambda ()
	;; Keep trying to store the incremented value until it works.
	(iter (for old next (car current))
	      (for new next (list (1+ (the sequence-number
					(car old)))))
	      (until (eq old (sb-ext:compare-and-swap
			      (car current) old new)))
	      (finally (return (car old))))))
  #-sbcl
  #.(error "Not implemented."))


;;; UUID utility functions
;;

(defun print-id (stream id colon? at?)
  "Print the UUID ID to STREAM. If COLON? is non-nil, all components
of ID are printed. Otherwise, just a block of 8 digits is printed.  "
  (declare (ignore at?))
  (cond
    ((null id)
     (format stream "    NOID"))
    ((or colon? (not (typep id 'uuid:uuid)))
     (format stream "~A" id))
    (t
     (format stream "~8,'0X" (slot-value id 'uuid::time-low)))))

(defmacro print-unreadable-id-object ((object stream &key (type t))
				      &body body)
  "Print OBJECT to STREAM in a manner similar to
`print-unreadable-object' but use the `id' slot of OBJECT as object
identity."
  (once-only (object stream)
    (with-unique-names (id-var)
      `(let ((,id-var (slot-value ,object 'id)))
	 (print-unreadable-object (,object ,stream :type ,type)
	   ,@body
	   (write-char #\Space stream)
	   (print-id ,stream ,id-var nil nil))))))


;;; UUID mixin
;;

(defclass uuid-mixin ()
  ((id :initarg  :id
       :type     uuid:uuid
       :initform (uuid:make-v4-uuid)
       :documentation
       ""))
  (:documentation
   "This class can be mixed into classes that need a uuid in there
instances."))

(defmethod initialize-instance :after ((instance uuid-mixin)
				       &key
				       (id nil id-supplied?))
  (when id-supplied?
    (setf (slot-value instance 'id)
	  (if (stringp id) (uuid:make-uuid-from-string id) id))))


;;; Scope mixin
;;

(defclass scope-mixin ()
  ((scope :type     scope
	  :documentation
	  "Stores the scope that is associated to the instance. For
long-lived and reused objects, interned scopes should be stored."))
  (:default-initargs
   :scope (missing-required-initarg 'scope-mixin :scope))
  (:documentation
   "This mixin class is intended to be mixed into classes instances of
which have a mandatory associated scope."))

(defmethod shared-initialize :after ((instance   scope-mixin)
				     (slot-names t)
				     &key
				     scope
				     (intern-scope? t))
  (when scope
    (setf (slot-value instance 'scope)
	  (make-scope scope :intern? intern-scope?))))


;;; URI mixin
;;

(defclass uri-mixin ()
  ((uri :type     puri:uri
	:documentation
	""))
  (:documentation
   "DOC"))

(defmethod initialize-instance :after ((instance uri-mixin)
				       &key
				       uri)
  (when uri
    (setf (slot-value instance 'uri) (puri:uri uri)))) ;; TODO intern the URI?


;;; Plist meta data mixin
;;

(defmacro define-plist-data-mixin (name
				   &key
				   (slot-name name))
  "Define a class `plist-NAME-mixin' which manages a plist in a
slot. Define the following accessors along with the class:
+ `NAME-count' :: Return number of items.
+ `NAME-keys' :: Return item keys.
+ `NAME-values' :: Return item values.
+ `NAME-plist' :: Return items as plist.
+ `NAME-alist' :: Return items as alist."
  (bind ((class-name (symbolicate "PLIST-" name "-MIXIN"))
	 (initarg    (make-keyword slot-name))
	 ((count-name keys-name values-name plist-name alist-name)
	  (map 'list (curry #'symbolicate name)
	       '("-COUNT" "-KEYS" "-VALUES" "-PLIST" "-ALIST"))))
    `(progn
       (defclass ,class-name ()
	 ((,slot-name :initarg  ,initarg
		      :type     list
		      :initform nil
		      :documentation

		      ,(format nil "Stores the ~(~A~) items associated ~
to the instance."
			       name)))
	 (:documentation
	  "This mixin adds storage for a plist of items and associated
accessors. See `define-plist-data-mixin' for a description."))

       (defgeneric ,count-name (object)
	 (:method ((object ,class-name))
	   (ash (length (slot-value object ',slot-name)) -1))
	 (:documentation
	  ,(format nil "Return the number of ~(~A~) items stored in OBJECT."
		   name)))

       (defgeneric ,keys-name (object)
	 (:method ((object ,class-name))
	   (iter (for (key) on (slot-value object ',slot-name)
		      :by #'cddr)
		 (collect key)))
	 (:documentation

	  ,(format nil "Return a list of the keys of ~(~A~) items ~
stored in OBJECT."
		   name)))

       (defgeneric ,values-name (object)
	 (:method ((object ,class-name))
	   (iter (for (key value) on (slot-value object ',slot-name)
		      :by #'cddr)
		 (collect value)))
	 (:documentation
	  ,(format nil "Return a list of the values of ~(~A~) items ~
stored in OBJECT."
		   name)))

       (defgeneric ,plist-name (object)
	 (:method ((object ,class-name))
	   (slot-value object ',slot-name))
	 (:documentation
	  ,(format nil "Return a plist of the ~(~A~) items stored in ~
OBJECT."
		   name)))

       (defgeneric ,alist-name (object)
	 (:method ((object ,class-name))
	   (plist-alist (slot-value object ',slot-name)))
	 (:documentation
	  ,(format nil "Return an alist of the ~(~A~) items stored ~
in OBJECT."
		   name)))

       (defgeneric ,name (object key)
	 (:method ((object ,class-name) (key t))
	   (getf (slot-value object ',slot-name) key))
	 (:documentation
	  ,(format nil "Return the ~(~A~) item of OBJECT identified ~
by KEY."
		   name)))

       (defgeneric (setf ,name) (new-value object key)
	 (:method ((new-value t) (object ,class-name) (key t))
	   (setf (getf (slot-value object ',slot-name) key) new-value))
	 (:documentation

	  ,(format nil "Associate NEW-VALUE to OBJECT as the ~(~A~)
item identified by KEY."
		   name))))))


;;; Utility functions
;;

(defun maybe-shorten-sequence (thing)
  (if (typep thing 'sequence)
      (let ((length (length thing)))
	(values (subseq thing 0 (min length 200)) (> length 200)))
      (values thing nil)))

(defun uuid= (left right)
  "Return non-nil if LEFT and RIGHT are equal UUIDs.
This function can be removed once the UUID system gets something
similar."
  (let ((bytes-left  (uuid:uuid-to-byte-array left))
	(bytes-right (uuid:uuid-to-byte-array right)))
    (declare (type (simple-array (unsigned-byte 8) (16))
		   bytes-left bytes-right))
    (equalp bytes-left bytes-right)))

(defun hostname ()
  (remove #\Newline
	  (with-output-to-string (stream)
	    (sb-ext:run-program "hostname" '()
				:search t
				:output stream))
	  :count 1 :from-end t))

(defmacro log1 (category object-or-message &rest args)
  "Log a message consisting of OBJECT-OR-MESSAGE and ARGS to the
category designated by the keyword CATEGORY.
If OBJECT-OR-MESSAGE is a string, it is assumed to be the message. In
this case, ARGS are assumed to be format arguments for the message
format string.
If OBJECT-OR-MESSAGE is not a string, it is assumed to be the object
at which the log message originates. In this case, the first element
of ARGS is assumed to be the message string and the remaining
arguments are assumed to be format arguments."
  (check-type category keyword)

  (bind ((category (intern (string category) :log5))
	 (package  (iter (for name in (cons (package-name *package*)
					    (package-nicknames *package*)))
			 (finding name minimizing (length name))))
	 (object   (unless (stringp object-or-message)
		     object-or-message))
	 ((message &rest args)
	  (if object
	      args
	      (cons object-or-message args))))
    (if object
	`(log5:with-context ,package
	   (log5:with-context ,object
	     (log5:log-for ,category ,message ,@args)))
	`(log5:with-context ,package
	   (log5:log-for ,category ,message ,@args)))))
