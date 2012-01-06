;;; conditions.lisp --- Conditions used in the patterns module of cl-rsb.
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

(cl:in-package :rsb.patterns)

(define-condition no-such-method (rsb-error)
  ((name :initarg  :name
	 :type     string
	 :reader   no-such-method-name
	 :documentation
	 "Stores the name of the method that was specified but could
not be found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The specified method ~S does not exist.~@:>"
	     (no-such-method-name condition))))
  (:documentation
   "This error is signaled when a specified method does not exist."))

(define-condition remote-call-failed (rsb-error
				      chainable-condition)
  ((method  :initarg  :method
	    :type     method
	    :reader   remote-call-failed-method
	    :documentation
	    "Stores the method of the failed call.")
   (request :initarg  :request
	    :reader   remote-call-failed-request
	    :documentation
	    "Stores the request object that was passed to the method
in the failed call."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to call method ~A with request ~
~A~/rsb::maybe-print-cause/~:@>"
	     (remote-call-failed-method  condition)
	     (remote-call-failed-request condition)
	     (chainable-condition-cause  condition))))
  (:documentation
   "This error is signaled when a remote method call fails for some
reason."))

(define-condition remote-method-execution-error (remote-call-failed)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Remote method ~A failed to execute for ~
request ~A~/rsb::maybe-print-cause/~:@>"
	     (remote-call-failed-method  condition)
	     (remote-call-failed-request condition)
	     (chainable-condition-cause  condition))))
  (:documentation
   "Error of this class are raised when a call to a remote method
succeeds in calling the method on the remote side but fails in the
actual remote method."))
