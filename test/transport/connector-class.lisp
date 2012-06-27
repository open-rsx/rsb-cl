;;; connector-class.lisp --- Unit tests for the connector-class class.
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

(cl:in-package :rsb.transport.test)

(deftestsuite connector-class-root (transport-root)
  ()
  (:documentation
   "Test suite for the `connector-class' class."))

(addtest (connector-class-root
          :documentation
	  "Test constructing an instance of `connector-class'.")
  construction/valid

  (eval
   '(defclass foo ()
      ((an-option :initarg  :an-option
		  :type     boolean
		  :initform t
		  :documentation
		  "doc"))
      (:metaclass connector-class)
      (:direction :in-push)
      (:wire-type string)
      (:schemas   :whoop)
      (:options
       (:an-option &slot))))

  (let ((class (find-class 'foo)))
    (ensure-same (connector-direction class) :in-push)
    (ensure-same (connector-wire-type class) 'string)
    (ensure-same (connector-schemas class)   '(:whoop))
    (ensure-same (connector-options class)   '((:an-option boolean
						:default     t
						:description "doc")))))

(addtest (connector-class-root
          :documentation
	  "Test error behavior for an invalid constructing an instance
of `connector-class'.")
  construction/invalid

  ;; We request the option no-such-option to be constructed from the
  ;; slot of the same name. Since there is no such slot, an error has
  ;; to be signaled.
  (ensure-condition 'error
    (eval '(progn
	    (defclass bar ()
	      ()
	      (:metaclass connector-class)
	      (:options
	       (:no-such-option &slot)))
	    (closer-mop:finalize-inheritance
	     (find-class 'bar))))))
