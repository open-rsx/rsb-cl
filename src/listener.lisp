;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
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

(defclass listener (receiving-client)
  ((handlers   :initarg  :handlers
	       :type     list
	       :accessor rsb.ep:handlers
	       :initform nil
	       :documentation
	       "Stores the list of handlers two which events received
by this listener should be dispatched.")
   (error-hook :initarg  :error-hook
	       :type     list
	       :initform nil
	       :documentation
	       "Stores a list of functions to call in case of
errors."))
  (:documentation
   "A listener consists of a set of filters, a set of handlers and has
a mechanism for dispatching matching events to these handlers."))

(defmethod (setf rsb.ep:handlers) :around ((new-value list)
					   (listener  listener))
  (let* ((configurator (rsb.ep:client-configurator listener))
	 (old-value    (rsb.ep:handlers listener))
	 (added        (set-difference new-value old-value))
	 (removed      (set-difference old-value new-value)))
    (prog1
	(call-next-method)
      (log1 :info listener "Added   handlers ~{~S~^, ~}" added)
      (log1 :info listener "Removed handlers ~{~S~^, ~}" removed)
      (iter (for handler in added)
	    (rsb.ep:notify configurator handler :handler-added))
      (iter (for handler in removed)
	    (rsb.ep:notify configurator handler :handler-removed)))))


;;; `listener' creation
;;

(defmethod make-listener ((scope scope)
			  &key
			  (transports (transport-options)))
  (handler-bind
      ;; Translate different kinds of errors into
      ;; `listener-creation-failed' errors.
      ((error #'(lambda (condition)
		  (error 'listener-creation-failed
			 :scope      scope
			 :transports transports
			 :cause      condition))))
    (make-participant 'listener scope :in-push transports)))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
