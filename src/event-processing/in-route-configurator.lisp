;;;; in-route-configurator.lisp --- Configurator for incoming event route.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass in-route-configurator (configurator)
  ((filters :type     list
            :accessor configurator-filters
            :initform '()
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

(defmethod notify ((configurator in-route-configurator)
                   (connector    t)
                   (action       (eql :connector-added)))
  (let+ (((&accessors-r/o (filters   configurator-filters)
                          (processor configurator-processor)) configurator))
    (call-next-method)

    ;; Notify new connector regarding filters.
    (iter (for filter in filters)
          (log:trace "~@<~A is adding filter ~A to ~A~@:>"
                     configurator filter connector)
          (unless (eq (notify connector filter :filter-added) :implemented)
            (pushnew filter (processor-filters processor))))

    ;; Notify PROCESSOR regarding added CONNECTOR and add PROCESSOR to
    ;; CONNECTOR's handlers.
    (notify processor connector action)

    (log:trace "~@<~A is connecting ~A -> ~A~@:>"
               configurator connector processor)
    (push processor (handlers connector))))

(defmethod notify ((configurator in-route-configurator)
                   (connector    t)
                   (action       (eql :connector-removed)))
  (let+ (((&accessors-r/o
           (filters   configurator-filters)
           (processor configurator-processor)) configurator))
    ;; Remove processor from connectors handlers and notify regarding
    ;; removed connector.
    (log:trace "~@<~A is disconnecting ~A -> ~A~@:>"
               configurator connector processor)
    (removef (handlers connector) processor)

    (notify processor connector action)

    ;; Notify remove connector regarding filters.
    (iter (for filter in filters)
          (log:trace "~@<~A is removing filter ~A from ~A~@:>"
                     configurator filter connector)
          (notify connector filter :filter-removed))

    (call-next-method)))

;;; Filters

(defmethod notify ((configurator in-route-configurator)
                   (filter       t)
                   (action       (eql :filter-added)))
  "Add FILTER from CONFIGURATOR's filter list and notify its
connectors and processor."
  (let+ (((&accessors
           (connectors configurator-connectors)
           (processor  configurator-processor)
           (filters    configurator-filters)) configurator))
    ;; Add FILTER to the filter list of CONFIGURATOR.
    (push filter filters)

    ;; Notify all connectors about the added filter. Unless all
    ;; connectors implemented the filter, add it to the processor.
    (case (reduce #'merge-implementation-infos connectors
                  :key (rcurry #'notify filter action))
      (:not-implemented
       (push filter (processor-filters processor))))

    ;; We implemented the filter either way.
    :implemented))

(defmethod notify ((configurator in-route-configurator)
                   (filter       t)
                   (action       (eql :filter-removed)))
  "Remove FILTER from CONFIGURATOR's filter list and notify its
connectors and processor."
  (let+ (((&accessors
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

(defmethod notify ((configurator in-route-configurator)
                   (handler      t)
                   (action       (eql :handler-added)))
  (let ((processor (configurator-processor configurator)))
    (log:trace "~@<~A is adding handler ~A to ~A~@:>"
               configurator handler processor)
    (push handler (handlers processor))))

(defmethod notify ((configurator in-route-configurator)
                   (handler      t)
                   (action       (eql :handler-removed)))
  (let ((processor (configurator-processor configurator)))
    (log:trace "~@<~A is removing handler ~A from ~A~@:>"
               configurator handler processor)
    (removef (handlers processor) handler)))
