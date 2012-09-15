;;; conditions.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.plugins)

(define-condition plugin-condition (condition)
  ((plugin :initarg  :plugin
	   :reader   plugin-condition-plugin
	   :documentation
	   ""))
  (:default-initargs
   :plugin (missing-required-initarg 'plugin-condition :plugin))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition plugin-error (error
				plugin-condition)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(define-condition no-such-plugin (plugin-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not find plugin ~S.~@:>"
	     (plugin-condition-plugin condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition plugin-function-error (plugin-error)
  ((function :initarg  :function
	     :reader   plugin-condition-function
	     :documentation
	     ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not find function ~A in plugin ~A.~@:>"
	     (plugin-condition-function condition)
	     (plugin-condition-plugin   condition))))
  (:documentation
   "TODO(jmoringe): document"))

(defun plugin-function-error (plugin function)
  "TODO(jmoringe): document"
  (error 'plugin-function-error
	 :plugin   plugin
	 :function function))
