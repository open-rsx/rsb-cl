;;; plugins.lisp ---
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

(defclass standard-plugin ()
  ((name :initarg  :name
	 :type     string
	 :reader   plugin-name
	 :documentation
	 ""))
  (:default-initargs
   :name (missing-required-initarg 'standard-plugin :name))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod resolve-function ((plugin standard-plugin)
			     (name   t))
  "TODO(jmoringe): document"
  (with-condition-translation
      (((error plugin-function-error)
	:plugin   plugin
	:function name))
    (let* ((package (format nil "RSB-~@:(~A~)" (plugin-name plugin)))
	   (symbol  (find-symbol name package)))
      (if symbol
	  (coerce symbol 'function)
	  (plugin-function-error plugin name)))))

(defmethod load! ((plugin standard-plugin))
  (funcall (resolve-function plugin +init-function-name+)))

(defmethod unload! ((plugin standard-plugin))
  (funcall (resolve-function plugin +shutdown-function-name+)))

(defmethod print-object ((object standard-plugin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A [~:[not loaded~;loaded~]]"
	    (plugin-name object)
	    (loaded?     object))))


;;;
;;

(defclass fasl-plugin (standard-plugin)
  ((pathname :initarg  :pathname
	     :type     pathname
	     :reader   plugin-pathname
	     :documentation
	     ""))
  (:default-initargs
   :pathname (missing-required-initarg 'fasl-plugin :pathname))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod loaded? ((plugin fasl-plugin))
  nil)

(defmethod load! ((plugin fasl-plugin))
  (load (plugin-pathname plugin))
  (call-next-method))


;;;
;;

(defclass asdf-plugin (standard-plugin)
  ((system ;; :type     pathname
	   :reader   plugin-system
	   :accessor %plugin-system
	   :documentation
	   ""))
  (:default-initargs
   :system (missing-required-initarg 'fasl-plugin :system)
   :name   "")
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   asdf-plugin)
                                     (slot-names t)
                                     &key
				     system)
  (setf (%plugin-system instance) (asdf:find-system system)))

(defmethod plugin-name ((plugin asdf-plugin))
  (asdf:component-name (plugin-system plugin)))

(defmethod loaded? ((plugin asdf-plugin))
  (asdf::system-loaded-p (plugin-system plugin)))

(defmethod load! ((plugin asdf-plugin))
  (asdf:load-system (plugin-system plugin))
  (call-next-method))
