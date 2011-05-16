;;; configurator.lisp ---
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

(in-package :rsb.event-processing)

(defclass configurator (scope-mixin)
  ((scope      :reader   configurator-scope)
   (connectors :initarg  :connectors
	       :type     list
	       :accessor configurator-connectors
	       :initform nil
	       :documentation
	       "")
   (processor  :initarg  :processor
	       :type     processor
	       :accessor configurator-processor
	       :initform (make-instance 'broadcast-processor)
	       :documentation
	       ""))
  (:documentation
   "DOC"))

(defmethod (setf configurator-connectors) :around ((new-value    list)
						   (configurator configurator))
  (bind ((old-value (configurator-connectors configurator)))
    (prog1
	(call-next-method)
      (let ((added   (set-difference new-value old-value))
	    (removed (set-difference old-value new-value)))
	(log1 :info "~@<~S added   connectors ~{~S~^, ~}~@:>" configurator added)
	(log1 :info "~@<~S removed connectors ~{~S~^, ~}~@:>" configurator removed)

	(iter (for connector in added)
	      (notify configurator connector :connector-added))
	(iter (for connector in removed)
	      (notify configurator connector :connector-removed))))))
