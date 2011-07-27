;;; conditions.lisp --- Conditions used in the patterns module of cl-rsb.
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

(define-condition remote-call-failed (rsb-error
				      chainable-condition)
  ((method  :initarg  :method
	    :type     method
	    :reader   remote-call-failed-method
	    :documentation
	    "Stores the method of the failed call.")
   (request :initarg  :request
	    :reader  remote-call-failed-request
	    :documentation
	    "Stores the request object that was passed to the method
in the failed call."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to call method ~A with request ~A~:@>"
	     (remote-call-failed-method  condition)
	     (remote-call-failed-request condition))
     (maybe-print-cause condition stream)))
  (:documentation
   "This error is signaled when a remote method call fails for some
reason."))
