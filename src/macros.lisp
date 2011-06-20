;;; marcos.lisp --- Convenience marcos for RSB-related functionality.
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

(in-package :rsb)

(defmacro with-listener ((var scope-or-uri
			  &rest args
			  &key &allow-other-keys)
			 &body body)
  "Execute BODY with VAR bound to an RSB listener for topic TOPIC on
channel CHANNEL. The listener is destroyed when the execution of BODY
ends normally or because of a control transfer."
  (check-type var symbol "a symbol")

  `(let ((,var (make-listener ,scope-or-uri ,@args)))
     (unwind-protect
	  (progn ,@body)
       (detach/ignore-errors ,var))))

(defmacro with-reader ((var scope-or-uri
			&rest args
			&key &allow-other-keys)
		       &body body)
  "Execute BODY with VAR bound to an RSB listener for topic TOPIC on
channel CHANNEL. The listener is destroyed when the execution of BODY
ends normally or because of a control transfer."
  (check-type var symbol "a symbol")

  `(let ((,var (make-reader ,scope-or-uri ,@args)))
     (unwind-protect
	  (progn ,@body)
       (detach/ignore-errors ,var))))

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

(defmacro with-informer ((var scope type
			  &rest args
			  &key &allow-other-keys)
			 &body body)
  "Execute BODY with VAR bound to a RSB publisher for name NAME. The
publisher is destroyed when executed of BODY ends normally or because
of a control transfer."
  (check-type var symbol "a symbol")

  `(let ((,var (make-informer ,scope ,type ,@args)))
     (unwind-protect
	  (progn ,@body)
       (detach/ignore-errors ,var))))
