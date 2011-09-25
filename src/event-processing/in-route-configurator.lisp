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

(defmethod collect-processor-mixins append ((configurator in-route-configurator))
  (ecase (configurator-direction configurator)
    (:in-push '(error-handling-dispatcher-mixin
		filtering-processor-mixin
		deliver-timestamp-mixin
		broadcast-processor))
    (:in-pull '(error-handling-dispatcher-mixin
		filtering-processor-mixin
		deliver-timestamp-mixin
		pull-processor))))


;;; Connectors
;;

(defmethod notify ((configurator in-route-configurator)
		   (connector    t)
		   (action       (eql :connector-added)))
  (bind (((:accessors-r/o (filters   configurator-filters)
			  (processor configurator-processor)) configurator))
    (call-next-method)

    ;; Notify new connector regarding filters.
    (iter (for filter in filters)
	  (log1 :info configurator "Adding filter ~S to ~S" filter connector)
	  (unless (eq (notify connector filter :filter-added) :implemented)
	    (pushnew filter (processor-filters processor))))

    ;; Notify PROCESSOR regarding added CONNECTOR and add PROCESSOR to
    ;; CONNECTOR's handlers.
    (notify processor connector action)

    (log1 :trace configurator "Connecting ~S -> ~S" connector processor)
    (push processor (handlers connector))))

(defmethod notify ((configurator in-route-configurator)
		   (connector    t)
		   (action       (eql :connector-removed)))
  (bind (((:accessors-r/o
	   (filters   configurator-filters)
	   (processor configurator-processor)) configurator))
    ;; Remove processor from connectors handlers and notify regarding
    ;; removed connector.
    (log1 :trace configurator "Disconnecting ~S -> ~S" connector processor)
    (removef (handlers connector) processor)

    (notify processor connector action)

    ;; Notify remove connector regarding filters.
    (iter (for filter in filters)
	  (log1 :info configurator "Removing filter ~S from ~S" filter connector)
	  (notify connector filter :filter-removed))

    (call-next-method)))


;;; Filters
;;

(defmethod notify ((configurator in-route-configurator)
		   (filter       t)
		   (action       (eql :filter-added)))
  "Add FILTER from CONFIGURATOR's filter list and notify its
connectors and processor."
  (bind (((:accessors
	   (connectors configurator-connectors)
	   (processor  configurator-processor)
	   (filters    configurator-filters)) configurator))
    ;; Add FILTER to the filter list of CONFIGURATOR.
    (push filter filters)

    ;; Notify all connectors about the added filter. Unless all
    ;; connectors implemented the filter, add it to the processor.
    (case (reduce #'merge-implementation-infos
		  (map 'list (rcurry #'notify filter action) connectors))
      (:not-implemented
       (push filter (processor-filters processor))))

    ;; We implemented the filter either way.
    :implemented))

(defmethod notify ((configurator in-route-configurator)
		   (filter       t)
		   (action       (eql :filter-removed)))
  "Remove FILTER from CONFIGURATOR's filter list and notify its
connectors and processor."
  (bind (((:accessors
	   (connectors configurator-connectors)
	   (processor  configurator-processor)
	   (filters    configurator-filters)) configurator))
    ;; Remove FILTER from PROCESSORS filter list.
    (removef (processor-filters processor) filter)

    ;; Notify all connectors about the removed filter. Implementation
    ;; info do not matter in this case.
    (map nil (rcurry #'notify filter action) connectors)

    ;; Remove FILTER from the filter list of CONFIGURATOR.
    (removef filters filter)

    :implemented))


;;; Handlers
;;

(defmethod notify ((configurator in-route-configurator)
		   (handler      t)
		   (action       (eql :handler-added)))
  (bind ((processor (configurator-processor configurator)))
    (log1 :trace configurator "Adding handler ~S to ~S" handler processor)
    (push handler (handlers processor))))

(defmethod notify ((configurator in-route-configurator)
		   (handler      t)
		   (action       (eql :handler-removed)))
  (bind ((processor (configurator-processor configurator)))
    (log1 :trace configurator "Removing handler ~S from ~S" handler processor)
    (removef (handlers processor) handler)))
