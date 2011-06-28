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
	       "Stores the list of connector instances the client uses
to access the bus.")
   (processor  :initarg  :processor
	       :reader   configurator-processor
	       :documentation
	       "Stores the processor instance that handles incoming or
outgoing events."))
  (:documentation
   "This class is intended to be used as a superclass of configurator
classes for specific directions. Every configurator instance has a
participant instance as its \"client\"."))

(defmethod shared-initialize :after ((instance   configurator)
                                     (slot-names t)
                                     &key
				     processor)
  (unless processor
    (setf (slot-value instance 'processor) (make-processor instance))))

(defmethod make-processor ((configurator configurator)
			   &rest args
			   &key &allow-other-keys)
  (apply #'make-instance
	 (ensure-processor-class (collect-processor-mixins configurator))
	 args))

(defmethod (setf configurator-connectors) :around ((new-value    list)
						   (configurator configurator))
  (bind ((old-value (configurator-connectors configurator)))
    (prog1
	(call-next-method)
      (let ((added   (set-difference new-value old-value))
	    (removed (set-difference old-value new-value)))
	(log1 :info configurator "Added   connectors ~{~S~^, ~}" added)
	(log1 :info configurator "Removed connectors ~{~S~^, ~}" removed)

	(iter (for connector in added)
	      (notify configurator connector :connector-added))
	(iter (for connector in removed)
	      (notify configurator connector :connector-removed))))))

(defmethod detach ((configurator configurator))
  "Detach all connectors from the scope of CONFIGURATOR."
  (log1 :info configurator "Detaching ~D connector~:P" (length (configurator-connectors configurator)))
  (iter (for connector in (configurator-connectors configurator))
	(restart-case
	    ;; Give each connector ten seconds to detach. If one takes
	    ;; longer, skip it.
	    #+sbcl (sb-ext:with-timeout 10
		     (notify configurator connector :connector-removed))
	    #-sbcl (notify configurator connector :connector-removed)
	  (ignore-error ()
	    :report "Ignore the error and continue with the remaining connectors."))))


;;; Connectors
;;

(defmethod notify ((configurator configurator)
		   (connector    t)
		   (action       (eql :connector-added)))
  (bind (((:accessors-r/o (scope configurator-scope)) configurator))
    (log1 :info configurator "Attaching connector ~S to scope ~S" connector scope)
    (notify connector scope :attached)))

(defmethod notify ((configurator configurator)
		   (connector    t)
		   (action       (eql :connector-removed)))
  (bind (((:accessors-r/o (scope configurator-scope)) configurator))
    (log1 :info configurator "Detaching connector ~S from ~S" connector scope)
    (notify connector scope :detached)))
