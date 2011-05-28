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
   (direction  :initarg  :direction
	       :type     direction
	       :reader   configurator-direction
	       :documentation
	       "The direction of the communication situation the
configurator is responsible for.")
   (connectors :initarg  :connectors
	       :type     list
	       :accessor configurator-connectors
	       :initform nil
	       :documentation
	       "")
   (processor  :initarg  :processor
	       :reader   configurator-processor
	       :documentation
	       ""))
  (:documentation
   "DOC"))

(defmethod shared-initialize :after ((instance   configurator)
                                     (slot-names t)
                                     &key
				     direction
				     processor)
  (unless processor
    (setf (slot-value instance 'processor)
	  (make-instance
	   (ensure-processor-class
	    (ecase direction
	      (:in-push '(filtering-processor-mixin
			  broadcast-processor))
	      (:in-pull '(filtering-processor-mixin
			  pull-processor))
	      (:out     '(broadcast-processor))))))))

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
