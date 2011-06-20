;;; in-route-configurator.lisp --- Configurator for incoming event route.
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

(defclass in-route-configurator (configurator)
  ((filters :type     list
	    :initform nil
	    :accessor configurator-filters
	    :documentation
	    "Stores the list of filters installed by the client."))
  (:documentation
   "Instances of this class configure in-direction connectors, filters
and an event process for a client that is an event receiving
participant."))

;; TODO this patterns occurs over and over
#|
(defmethod (setf configurator-filters) :around ((new-value    list)
						(configurator in-route-configurator))
  "Notify interested parties of the change in the set of
listeners."
  (bind (((:slots-r/o in-connectors (old-value listeners)) channel)
	 ((:flet connect-handler (connector listener))
	  (hooks:add-to-hook (hooks:object-hook connector 'push-hook)
			     (curry #'push1 listener))))
    (prog1
	(call-next-method)
      (let ((added   (set-difference new-value old-value))
	    (removed (set-difference old-value new-value))) ;; TODO we could use hooks for this
	(log5:log-for (or rsb log5:info) "~@<~S added   filters ~{~S~^, ~}~@:>" channel added)
	(log5:log-for (or rsb log5:info) "~@<~S removed filters ~{~S~^, ~}~@:>" channel removed)


	(map-product #'connect-handler          in-connectors added)
	;; TODO disconnect
	(map-product (rcurry #'notify :removed) in-connectors removed)
	))))
|#


;;; Connectors
;;

(defmethod notify ((configurator in-route-configurator)
		   (connector    t
				 ;; rsb.transport::connector
				 )
		   (action       (eql :connector-added)))
  (bind (((:accessors-r/o
	   (scope     configurator-scope)
	   (filters   configurator-filters)
	   (processor configurator-processor)) configurator))
    ;; Notify new connector regarding scope and filters.
    (log1 :info "~S attaching connector ~S to ~S" configurator connector scope)
    (notify connector scope :attached)

    (iter (for filter in filters)
	  (log1 :info "~S adding filter ~S to ~S" configurator filter connector)
	  (notify connector filter :added))

    ;; Notify processor regarding new connector and add to connectors
    ;; handlers.
    (notify processor connector action)

    (log1 :info "~S connecting ~S -> ~S" configurator connector processor)
    (push processor (handlers connector))))

(defmethod notify ((configurator in-route-configurator)
		   (connector    t ;; rsb.transport::connector
				 )
		   (action       (eql :connector-removed)))
  (bind (((:accessors-r/o
	   (scope     configurator-scope)
	   (filters   configurator-filters)
	   (processor configurator-processor)) configurator))
    ;; Remove processor from connectors handlers and notify regarding
    ;; removed connector.
    (log1 :info "~S disconnecting ~S -> ~S" configurator connector processor)
    (removef (handlers connector) processor)

    (notify processor connector action)

    ;; Notify remove connector regarding filters and scope.
    (iter (for filter in filters)
	  (log1 :info "~S removing filter ~S to ~S" configurator filter connector)
	  (notify connector filter :removed))

    (log1 :info "~S detaching connector ~S to ~S" configurator connector scope)
    (notify connector scope :detached)))


;;; Filters
;;

(defmethod notify ((configurator in-route-configurator)
		   (filter       t)
		   (action       (eql :filter-added)))
  "Remove FILTER from CONFIGURATOR's filter list and notify its
connectors."
  (push filter (configurator-filters configurator))

  (reduce #'merge-implementation-infos
	  (map 'list (rcurry #'notify filter :added)
	       (configurator-connectors configurator))))

#|
 (defmethod notify ((configurator in-route-configurator)
		   (filter       filter)
		   (action       t))
  "DOC"
  (map 'nil (rcurry #'notify filter action) in-connectors))
|#


;;; Handlers
;;

(defmethod notify ((configurator in-route-configurator)
		   (handler      t)
		   (action       (eql :handler-added)))
  (bind ((processor (configurator-processor configurator)))
    (log1 :info "~S adding handler ~S to ~S" configurator handler processor)
    (push handler (handlers processor))))

(defmethod notify ((configurator in-route-configurator)
		   (handler      t)
		   (action       (eql :handler-removed)))
  (bind ((processor (configurator-processor configurator)))
    (log1 :info "~S removing handler ~S from ~S" configurator handler processor)
    (removef (handlers processor) handler)))
