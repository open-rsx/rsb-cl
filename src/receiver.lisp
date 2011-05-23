;;; receiver.lisp --- Pull-based receiving participant class.
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

(defclass receiver (participant
		    rsb.ep:client)
  ()
  (:documentation
   "Instances of this class provide a pull-based interface for
receiving events."))

(defmethod receive ((receiver receiver)
		    &key
		    (block? t))
  ;; TODO how to handle multiple connectors?
  ;; TODO who should be responsible for calling emit on the connector?
  (let ((connector (first
		    (rsb.ep:configurator-connectors
		     (rsb.ep:client-configurator receiver)))))
    (rsb.ep:emit connector block?)))

;; TODO maybe restart should be installed in transport layer
(defmethod receive :around ((receiver receiver)
			    &key &allow-other-keys)
  (let (result)
    (tagbody
     skip
       (restart-case ;; TODO the restart-case is expensive
	   (setf result (call-next-method))
	 (skip ()
	   :report (lambda (stream)
		     (format stream "~@<Skip event.~@:>"))
	   (go skip))))
    result))


;;; `receiver' creation
;;

(defmethod make-receiver ((scope scope)
			  &key
			  (transports (transport-options)))
  "TODO(jmoringe): document"
  (let* ((configurator (make-instance 'rsb.ep:in-route-configurator
				      :scope scope))
	 (connectors   (funcall (fdefinition (find-symbol "MAKE-CONNECTORS" :rsb.transport)) ;; TODO
				transports :in-pull))
	 (receiver     (make-instance 'receiver
				      :scope        scope
				      :configurator configurator)))

    (unless (length= 1 connectors)
      (error "Receiver currently only works with a single connectors")) ;; TODO

    (setf (rsb.ep:configurator-connectors configurator) connectors)

    receiver))

(define-participant-creation-uri-methods receiver (scope puri:uri))

(define-participant-creation-restart-method receiver (scope scope))
(define-participant-creation-restart-method receiver (scope puri:uri))
