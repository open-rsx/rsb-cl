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
  ((error-hook :initarg  :error-hook
	       :type     list
	       :initform nil
	       :documentation
	       "Stores a list of functions to call in case of
errors."))
  (:documentation
   "A listener consists of a set of filters, a set of handlers and has
a mechanism for dispatching matching events to these handlers."))

(defmethod rsb.ep:handlers ((listener listener))
  ;(rsb.ep:handlers (rsb.ep:client-configurator listener))
  nil)

(defmethod (setf rsb.ep:handlers) ((new-value list) (listener listener))
  (rsb.ep:notify (rsb.ep:client-configurator listener) (first new-value) :handler-added))
  ;(setf (rsb.ep:handlers (rsb.ep:client-configurator listener))
;	new-value))


;;; `listener' creation
;;

(defmethod make-listener ((scope scope)
			  &key
			  (transports (transport-options)))
  (make-participant 'listener scope :in-push  transports))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
