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


;;; UUID utility functions
;;

(defun print-id (stream id colon? at?)
  "Print the UUID ID to STREAM. If COLON? is non-nil, all components
of ID are printed. Otherwise, just a block of 8 digits is printed.  "
  (declare (ignore at?))
  (if colon?
      (format stream "~A" id)
    (format stream "~8,'0X" (slot-value id 'uuid::time-low))))

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
  ((scope :initarg :scope
	  :type     scope
	  :documentation
	  ""))
  (:default-initargs
   :scope (missing-required-initarg 'scope-mixin :scope))
  (:documentation
   "DOC"))

(defmethod initialize-instance :after ((instance scope-mixin)
                                       &key
				       scope)
  (setf (slot-value instance 'scope)
	(make-scope scope)))


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

(defclass plist-meta-data-mixin ()
  ((meta-data :initarg  :meta-data
	      :type     list
	      :initform nil
	      :documentation
	      ""))
  (:documentation
   "DOC"))

(defmethod meta-data-count ((object plist-meta-data-mixin))
  (ash (length (slot-value object 'meta-data)) -1))

(defmethod meta-data-keys ((object plist-meta-data-mixin))
  (iter (for (key) on (slot-value object 'meta-data)
	     :by #'cddr)
	(collect key)))

(defmethod meta-data-values ((object plist-meta-data-mixin))
  (iter (for (key value) on (slot-value object 'meta-data)
	     :by #'cddr)
	(collect value)))

(defmethod meta-data-plist ((object plist-meta-data-mixin))
  (slot-value object 'meta-data))

(defmethod meta-data-alist ((object plist-meta-data-mixin))
  (plist-alist (slot-value object 'meta-data)))

(defmethod meta-data ((object plist-meta-data-mixin) (key t))
  (getf (slot-value object 'meta-data) key))

(defmethod (setf meta-data) ((new-value t) (object plist-meta-data-mixin) (key t))
  (setf (getf (slot-value object 'meta-data) key) new-value))


;;; Utility functions
;;

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
