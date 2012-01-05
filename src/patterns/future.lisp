;;; future.lisp --- A simple implementation of the future pattern.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(in-package :rsb.patterns)


;;; Representation of errors
;;

(defconstant +future-failure-marker+
  (if (boundp '+future-failure-marker+)
      (symbol-value '+future-failure-marker+)
      (gensym "FUTURE-FAILURE-MARKER"))
  "This unique object is used to mark failure values as such and
distinguish them from regular results.")

(deftype future-failure-value ()
  "The extension of this type consists of all failure values."
  `(cons (eql ,+future-failure-marker+)
	 (cons keyword
	       (cons t null))))

(declaim (inline future-failure-tag future-failure-condition))

(defun future-failure-tag (value)
  "Return the tag of a failure value."
  (second value))

(defun future-failure-condition (value)
  "Return the condition data of a failure value."
  (third value))


;;; `future' class
;;

(defclass future ()
  ((result    :initarg  :result
	      :writer   (setf %future-result)
	      :documentation
	      "Stores the result of the operation associated to the
future. Remains unbound until the operation completes successfully or
fails.")
   (lock      :initarg  :lock
	      :reader   %future-lock
	      :initform (bt:make-lock "Future lock")
	      :documentation
	      "Stores the lock that protects access to the result
slot.")
   (condition :initarg  :condition
	      :reader   %future-condition
	      :initform (bt:make-condition-variable
			 :name "Future condition")
	      :documentation
	      "Stores the condition variable which can be used to wait
for the result slot to be set."))
  (:documentation
   "Instances of this class represent results of operations that are
still in progress when the respective instances are made. Instances
can therefore be considered placeholders for the actual results which
may or may not (when the producing operation fails) become available
later.

Interaction with `future' instances is done using methods on the
following protocol functions:
+ `future-done?' :: Check whether the associated operation finished or
    failed.
+ `future-result' :: Obtained the result, potentially waiting for it
    to become available.
+ `(setf future-result)' :: Supply a result for the `future' instance.
+ `(setf future-error)' :: Indicate that the operation associated to
    the `future' instance failed.

It is possible to supply values for the result, lock and condition
slots of new `future' instance using initargs. The former may be
useful when a result is immediately available but the future protocol
has to be obeyed. The latter two may be useful when the lock and
condition objects have to be available to some code outside the future
or for performance reasons."))

(defmethod future-done? ((future future))
  (bt:with-lock-held ((%future-lock future))
    (when (slot-boundp future 'result)
      (let ((value (slot-value future 'result)))
	;; When there is some result, check whether it indicates an
	;; error.
	(if (typep value 'future-failure-value)
	    (future-failure-tag value)
	    :done)))))

(defmethod future-result :around ((future future)
				  &key
				  timeout
				  (error? t))
  (cond
    ;; If TIMEOUT has not been supplied, avoid the overhead and just
    ;; call the next method.
    ((null timeout)
     (call-next-method))

    ;; If TIMEOUT has been supplied and errors should be signaled,
    ;; only wait for the specified amount of time and let the timeout
    ;; condition through, should it be signaled.
    (error?
     (bt:with-timeout (timeout)
       (call-next-method)))

    ;; If TIMEOUT has been supplied, but error signaling is not
    ;; desired, handle timeout conditions turning them into result
    ;; values.
    (t
     (handler-case
	 (bt:with-timeout (timeout)
	   (call-next-method))
       (bt:timeout (condition)
	 (declare (ignore condition))
	 (values nil :timeout))))))

(defmethod future-result ((future future)
			  &key
			  (error? t)
			  &allow-other-keys)
  (bind (((:accessors-r/o (lock      %future-lock)
			  (condition %future-condition)) future)
	 (value (progn
		  (bt:with-lock-held (lock)
		    (iter (until (slot-boundp future 'result))
			  (bt:condition-wait condition lock)))
		  (slot-value future 'result))))
    (%dispatch-result value error?)))

(defmethod (setf future-result) ((new-value t) (future future))
  (bt:with-lock-held ((%future-lock future))
    (prog1
	(setf (%future-result future) new-value)
      (bt:condition-notify (%future-condition future)))))

(defmethod (setf future-error) ((new-value t) (future future))
  ;; Store a result value that indicates an error. NEW-VALUE can be
  ;; used to (construct and) signal the error.
  (setf (future-result future) (%make-failure-value
				(ensure-list new-value))))

(defmethod print-object ((object future) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (or (future-done? object) :running))))


;;; Cons cells as futures
;;

(defmethod future-done? ((future cons))
  (if (typep (car future) 'future-failure-value)
      (future-failure-tag (car future))
      :done))

(defmethod future-result ((future cons)
			  &key
			  (error? t)
			  &allow-other-keys)
  (%dispatch-result (car future) error?))

(defmethod (setf future-result) ((new-value t) (future cons))
  (setf (car future) new-value))

(defmethod (setf future-error) ((new-value t) (future cons))
  (setf (car future) (%make-failure-value (ensure-list new-value))))


;;; Utility functions
;;

(defun %dispatch-result (value error?)
  "Return the result stored in VALUE or signal an error, depending on
ERROR?"
  (if (typep value 'future-failure-value)
      ;; When the stored value indicates an error, signal the error or
      ;; return the tag, depending on ERROR?.
      (if error?
	  (apply #'error (future-failure-condition value))
	  (values nil (future-failure-tag value)))
      (values value :done)))

(defun %make-failure-value (condition-data)
  "Return an object that can be used to indicate a failed operation
for a condition described by CONDITION-DATA."
  (list +future-failure-marker+ :failed condition-data))
