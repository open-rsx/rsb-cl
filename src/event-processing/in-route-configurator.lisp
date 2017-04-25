;;;; in-route-configurator.lisp --- Configurator for incoming event route.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
   "Configure in-direction connectors, filters and a processor for a client.

    The client generally is an event receiving participant."))

(defmethod collect-processor-mixins append ((configurator in-route-configurator))
  `(error-policy-handler-mixin
    restart-handler-mixin
    restart-dispatcher-mixin
    filtering-processor-mixin
    deliver-timestamp-mixin
    ,(ecase (configurator-direction configurator)
       (:in-push 'broadcast-processor)
       (:in-pull 'pull-processor))))

;;; Connectors

(defmethod notify ((configurator in-route-configurator)
                   (connector    t)
                   (action       (eql :connector-added)))
  (let+ (((&structure-r/o configurator- filters processor) configurator))
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
  (let+ (((&structure-r/o configurator- filters processor) configurator))
    ;; Remove processor from CONNECTOR's handler list and notify
    ;; processor regarding removed CONNECTOR.
    (log:trace "~@<~A is disconnecting ~A -> ~A~@:>"
               configurator connector processor)
    (removef (handlers connector) processor :count 1)

    (notify processor connector action)

    ;; Notify CONNECTOR regarding removed filters.
    (iter (for filter in filters)
          (log:trace "~@<~A is removing filter ~A from ~A~@:>"
                     configurator filter connector)
          (notify connector filter :filter-removed))

    (call-next-method)))

;;; Filters

(defmethod notify ((configurator in-route-configurator)
                   (filter       t)
                   (action       (eql :filter-added)))
  ;; Add FILTER to CONFIGURATOR's filter list and notify its
  ;; connectors and processor.
  (let+ (((&structure configurator- connectors processor filters)
          configurator))
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
  ;; Remove FILTER from CONFIGURATOR's filter list and notify its
  ;; connectors and processor.
  (let+ (((&structure configurator- connectors processor filters)
          configurator))
    ;; Remove FILTER from PROCESSOR's filter list.
    (removef (processor-filters processor) filter)

    ;; Notify all connectors about the removed FILTER. Implementation
    ;; info do not matter in this case.
    (map nil (rcurry #'notify filter action) connectors)

    ;; Remove FILTER from the filter list of CONFIGURATOR.
    (removef filters filter :count 1)

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
