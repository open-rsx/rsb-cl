;;; error-handling-dispatcher-mixin.lisp --- Unit tests for the error-handling-dispatcher-mixin class.
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

(in-package :rsb.event-processing.test)

(deftestsuite error-handling-dispatcher-mixin-root (event-processing-root)
  ((simple-processor (make-instance (ensure-processor-class
				     '(error-handling-dispatcher-mixin
				       broadcast-processor))))
   (simple-event     (make-event "/" "bla")))
  (:documentation
   "Test suite for the `error-handling-dispatcher-mixin' class."))

(defun signaling-handler (event)
  "A handler that unconditionally signals an error."
  (error "~@<I hate ~A.~@:>" event))

(addtest (error-handling-dispatcher-mixin-root
          :documentation
	  "Test basic error handling policies of the
`error-handling-dispatcher-mixin' class.")
  smoke

  (push #'signaling-handler (handlers simple-processor))

  ;; Error policy nil means to just unwind.
  (setf (processor-error-policy simple-processor) nil)
  (ensure-condition 'simple-error
    (handle simple-processor simple-event))

  ;; The error policy #'continue should prevent the error from being
  ;; signaled.
  (setf (processor-error-policy simple-processor) #'continue)
  (handle simple-processor simple-event)

  ;; The error policy #'log-error should prevent the error from being
  ;; signaled.
  (setf (processor-error-policy simple-processor) #'log-error)
  (handle simple-processor simple-event))
