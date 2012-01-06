;;; marcos.lisp --- Convenience marcos for RSB-related functionality.
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

(in-package :rsb)

(defmacro define-with-participant-macro (kind &rest extra-args)
  (let ((name      (symbolicate "WITH-" kind))
	(make-name (symbolicate "MAKE-" kind)))
    `(defmacro ,name ((var scope-or-uri ,@extra-args
		       &rest args
		       &key
		       transports
		       converters
		       &allow-other-keys)
		      &body body)

       ,(format nil "Execute BODY with VAR bound to a `~(~A~)' for ~
the channel designated by SCOPE-OR-URI. The ~:*~(~A~) is destroyed ~
when the execution of BODY ends normally or because of a control ~
transfer."  kind)
       (declare (ignore transports converters))
       (check-type var symbol "a symbol")

       `(let ((,`,var (,',make-name ,`,scope-or-uri
				    ,,@(mapcar #'(lambda (arg) `,arg)
					       extra-args)
				    ,@`,args)))
	  (unwind-protect
	       (progn ,@`,body)
	    (detach/ignore-errors ,`,var))))))

(define-with-participant-macro listener)
(define-with-participant-macro reader)
(define-with-participant-macro informer type)

(defmacro with-handler (listener
			((event-var) &body handler-body)
			&body body)
  "Execute BODY with LISTENER enabled."
  (check-type event-var symbol "a symbol")

  (once-only (listener)
    (with-unique-names (handler-var)
      `(let ((,handler-var (function (lambda (,event-var)
			     ,@handler-body))))
	 (unwind-protect
	      (progn
		(push ,handler-var (rsb.ep:handlers ,listener))
		,@body)
	   (removef (rsb.ep:handlers ,listener) ,handler-var))))))
