;;; conditions.lisp --- Conditions used by the event-processing module.
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package :rsb.event-processing)

(define-condition transform-error (error
				   chainable-condition)
  ((transform :initarg  :transform
	      :reader   transform-error-transform
	      :documentation
	      "Stores the failed transformed.")
   (object    :initarg  :object
	      :reader   transform-error-object
	      :documentation
	      "Stores the object for which the transform failed."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-error :transform)
   :object    (missing-required-initarg 'transform-error :object))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not apply transform ~A to ~
~A~/more-conditions::maybe-print-cause/~@:>"
	     (transform-error-transform condition)
	     (transform-error-object    condition)
	     condition)))
  (:documentation
   "This error is signaled when a transform fails."))
