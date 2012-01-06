;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
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

(cl:in-package :rsb)

(defclass listener (receiving-client)
  ((handlers :initarg  :handlers
	     :type     list
	     :accessor rsb.ep:handlers
	     :initform nil
	     :documentation
	     "Stores the list of handlers two which events received by
this listener should be dispatched."))
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
			  (transports (transport-options))
			  (converters (default-converters)))
  (handler-bind
      ;; Translate different kinds of errors into
      ;; `listener-creation-failed' errors.
      ((error #'(lambda (condition)
		  (error 'listener-creation-failed
			 :scope      scope
			 :transports transports
			 :cause      condition))))
    (make-participant 'listener scope :in-push transports converters)))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
