;;; error-policy-mixin.lisp --- Unit tests for the error-policy-mixin class.
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

(in-package :rsb.event-processing.test)

(deftestsuite error-policy-mixin-root (event-processing-root)
  ((simple-processor (make-instance (ensure-processor-class
				     '(error-policy-mixin
				       broadcast-processor))))
   (simple-event     (make-event "/" "bla")))
  (:documentation
   "Test suite for the `error-policy-mixin' class."))

(defun signaling-handler (event)
  "A handler that unconditionally signals an error."
  (error "~@<I hate ~A.~@:>" event))

(addtest (error-policy-mixin-root
          :documentation
	  "Test basic error handling policies of the
`error-policy-mixin' class.")
  smoke

  (push #'signaling-handler (handlers simple-processor))

  ;; Error policy nil means to just unwind.
  (setf (processor-error-policy simple-processor) nil)
  (ensure-condition 'simple-error
    (handle simple-processor simple-event))

  ;; Error policy nil means to just unwind.
  (setf (processor-error-policy simple-processor) #'ignore-error)
  (handle simple-processor simple-event)

  ;; Error policy nil means to just unwind.
  (setf (processor-error-policy simple-processor) #'log-error)
  (handle simple-processor simple-event))
