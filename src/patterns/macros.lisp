;;; macros.lisp --- Convenience macros provided by the patterns module.
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

(in-package :rsb.patterns)

(define-with-participant-macro remote-server)
(define-with-participant-macro local-server)

(defmacro with-methods ((var) methods &body body)
  "Execute body with the methods defined by METHODS added to the
server that is the value of VAR. METHODS is a list of items of the form
\(NAME (ARG REQUEST-TYPE) BODY)
where NAME is the name of the method, ARG is a symbol which will be
bound to the request data during the execution of the method body
BODY. REQUEST-TYPE specifies the type of acceptable requests. If
REQUEST-TYPE is the keyword :event, BODY is called with ARG bound to
the request event (instead of just the payload)."
  (check-type var symbol "a symbol")

  (bind (((:flet process-one (spec))
	  (bind (((name (arg type) &rest body) spec)
		 (name-as-string (string name)))
	    (check-type name-as-string method-name "a valid method name")
	    (check-type arg            symbol      "a symbol")

	    (list
	     ;; add method
	     `(setf (server-method ,var ,name-as-string
				   ,@(when (eq type :event)
				       '(:argument :event)))
		    #'(lambda (,arg) ,@body))
	     ;; remove method
	     `(let ((method (server-method ,var ,name-as-string :error? nil)))
		(when method
		  (handler-bind
		      (((or error bt:timeout)
			#'(lambda (condition)
			    (warn "~@<Error removing method ~S: ~A~@:>"
				  method condition)
			    (continue))))
		    (%remove-method-with-restart-and-timeout
		     ,var method)))))))
	 (add-and-remove (map 'list #'process-one methods)))
    `(unwind-protect
	  (progn
	    ,@(map 'list #'first add-and-remove)
	    ,@body)
       ,@(map 'list #'second add-and-remove))))
