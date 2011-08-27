;;; sometimes-interruptible-mixin.lisp --- A threaded receiver that is sometimes interruptible.
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

(in-package :rsb.transport)

(defclass sometimes-interruptible-mixin ()
  ((interruptible? :initarg  :interruptible?
		   :accessor connector-interruptible?
		   :initform (cons :interruptible nil)
		   :documentation
		   "Stores the interruptibility state of the
receiver. One of interruptible, uninterruptible and interrupting."))
  (:documentation
   "This class is intended to be mixed into threaded receiver classes
that cannot always be interrupted."))

(defmethod connector-interruptible? ((connector sometimes-interruptible-mixin))
  (car (slot-value connector 'interruptible?)))

(defmethod (setf connector-interruptible?) ((new-value symbol)
					    (connector sometimes-interruptible-mixin))
  (let ((precondition (ecase new-value
			(:interruptible   :uninterruptible)
			(:uninterruptible :interruptible)
			(:interrupting    :interruptible)))
	(cell         (slot-value connector 'interruptible?)))
    (iter (until (eq (sb-ext:compare-and-swap
		      (car cell) precondition new-value)
		     precondition))))
  new-value)

(defmethod stop-receiver :around ((connector sometimes-interruptible-mixin))
  (unwind-protect
       (progn
	 (setf (connector-interruptible? connector) :interrupting)
	 (call-next-method))
    (setf (car (slot-value connector 'interruptible?)) :interruptible)))

(defmethod handle :around ((connector sometimes-interruptible-mixin)
			   (event     t))
  (prog2
      (setf (connector-interruptible? connector) :uninterruptible)
      (call-next-method)
    (setf (connector-interruptible? connector) :interruptible)))
