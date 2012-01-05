;;; sometimes-interruptible-mixin.lisp --- A threaded receiver that is sometimes interruptible.
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
