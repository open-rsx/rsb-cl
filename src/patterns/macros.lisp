;;; macros.lisp --- Convenience macros provided by the patterns module.
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

(defmacro with-remote-server ((var scope
			       &rest args
			       &key &allow-other-keys)
			      &body body)
  "Execute BODY with VAR bound to a RSB publisher for name NAME. The
publisher is destroyed when executed of BODY ends normally or because
of a control transfer."
  (check-type var symbol "a symbol")

  `(let ((,var (make-remote-server ,scope ,@args)))
     (unwind-protect
	 (progn ,@body)
       (detach/ignore-errors ,var))))

(defmacro with-local-server ((var scope
			      &rest args
			      &key &allow-other-keys)
			     &body body)
  "Execute BODY with VAR bound to a RSB publisher for name NAME. The
publisher is destroyed when executed of BODY ends normally or because
of a control transfer."
  (check-type var symbol "a symbol")

  `(let ((,var (make-local-server ,scope ,@args)))
     (unwind-protect
	 (progn ,@body)
       (detach/ignore-errors ,var))))

(defmacro with-methods ((var) methods &body body)
  "Execute body with the methods defined by METHODS added to the
server that is the value of VAR. METHODS is a list of items of the form
\(NAME (ARG TYPE) BODY)
where NAME is the name of the method, ARG is a symbol which will be
bound to the request data during the execution of the method body
BODY. TYPE specifies the type of acceptable requests."
  (check-type var symbol "a symbol")

  (bind (((:flet add-one (spec))
	  (bind (((name (arg type) &rest body) spec))
	    (check-type name (or symbol string) "a symbol or a string")
	    (check-type arg  symbol             "a symbol")

	    `(setf (server-method ,var ,(string name))
		   #'(lambda (,arg) ,@body))))
	 ((:flet remove-one (spec))
	  (bind (((name &rest _) spec))
	    `(setf (server-method ,var ,(string name)) nil))))
    `(unwind-protect
	  (progn ,@(map 'list #'add-one methods) ,@body)
       ,@(map 'list #'remove-one methods))))
