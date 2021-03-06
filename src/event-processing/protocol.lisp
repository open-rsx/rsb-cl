;;;; protocol.lisp --- Protocols provided by the event-processing module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

;;; Push source protocol
;;;
;;; This protocol is provided by event sources that emit events
;;; without requiring an external trigger, for example a thread
;;; polling a network connection.

(defgeneric handlers (source)
  (:documentation
   "Return the handlers associated to SOURCE."))

(defgeneric (setf handlers) (new-value source)
  (:documentation
   "Set the list of handlers associated to SOURCE to NEW-VALUE."))

;;; Pull source protocol
;;;
;;; This protocol is provided by event sources that emit events only
;;; after being triggered externally. This external triggering is
;;; achieve by calling `emit' with such a source.

(defgeneric emit (source block?)
  (:documentation
   "Block until an event is available from SOURCE."))

;;; Sink protocol

(defgeneric handle (sink data)
  (:documentation
   "Put DATA into SINK.

    SINK may for example process, relay or discard DATA."))

;;; Default behavior

(defmethod handle ((sink function) (data t))
  ;; If SINK is a function, call it with DATA.
  (funcall sink data))

(defmethod handle ((sink list) (data t))
  ;; If SINK is a list, treat it as a list of sinks and let each
  ;; contained sink handle DATA.
  (map nil (rcurry #'handle data) sink))

;;; Dispatching processor protocol

(defgeneric dispatch (processor event)
  (:documentation
   "Dispatch EVENT in the manner implemented by PROCESSOR.

    This may imply calling some handlers, for example."))

;;; Processor access protocol
;;;
;;; This protocol allows determining which parts of events are not
;;; accessed (read or written) by given processes (such as filters,
;;; transforms, or converters) and thus potentially omitting creating,
;;; transmitting or (de)serializing these parts.

(defvar *event-parts*
  '((:scope           . (:accessors ((event-scope           ()    (:read :write)))))
    (:origin          . (:accessors ((event-origin          ()    (:read :write))
                                     (event-id              ()    (:read       )))))
    (:sequence-number . (:accessors ((event-sequence-number ()    (:read :write))
                                     (event-id              ()    (:read       )))))
    (:method          . (:accessors ((event-method          ()    (:read :write)))))
    (:data            . (:accessors ((event-data            ()    (:read :write)))))
    (:meta-data       . (:accessors ((rsb:meta-data-plist   ()    (:read       ))
                                     (rsb:meta-data-alist   ()    (:read       ))
                                     (rsb:meta-data         (key) (:read :write)))))
    (:timestamp       . (:accessors ((rsb:timestamp-plist   ()    (:read       ))
                                     (rsb:timestamp-alist   ()    (:read       ))
                                     (rsb:timestamp         (key) (:read :write)))))
    (:causes          . (:accessors ((event-causes          ()    (:read :write))))))
  "Parts of events with access and mode information.

   Entries are of the form:

     (PART . (:accessors ACCESSORS)

   where PART is a symbol naming the part and ACCESSORS is a list of
   elements of the form

     (ACCESSOR-NAME LAMBDA-LIST MODES)

   where ACCESSOR-NAME is the name of the read function for accessing
   PART of an event and LAMBDA-LIST is the lambda-list of that
   function. MODES is a list containing one or both of :read
   and :write.")

(defgeneric access? (processor part mode)
  (:documentation
   "Return true if PROCESSOR needs MODE access to PART of events."))

;; Default behavior

(defmethod access? ((processor t) (part t) (mode t))
  ;; Since we do not know which PART and MODE values will be defined
  ;; in the future, we can only default to false.
  ;;
  ;; This default behavior is slightly dangerous because processor
  ;; code that is not aware of the access protocol will claim not
  ;; needing any parts.
  nil)

(defmethod access? ((processor sequence) (part t) (mode t))
  (some (rcurry #'access? part mode) processor))

(defmethod access? ((processor t) (part sequence) (mode t))
  (some (lambda (part) (access? processor part mode)) part))

;;; Filterting processor protocol

(defgeneric processor-filters (processor)
  (:documentation
   "Return the list of filters applied by PROCESSOR."))

(defgeneric (setf processor-filters) (new-value processor)
  (:documentation
   "Set the list of filters applied by PROCESSOR to NEW-VALUE."))

;;; Notification protocol

(defgeneric notify (recipient subject action)
  (:documentation
   "When ACTION is either :filter-added or :filter-removed, methods
    should return one of the symbols :not-implemented or :implemented
    to indicate whether the combination of the filter SUBJECT and
    ACTION could be implemented by RECIPIENT."))

;;; Default behavior

(defmethod notify ((recipient t) (subject t) (action t))
  ;; The default behavior is to do nothing.n
  (values))

(defmethod notify ((recipient t)
                   (subject   t)
                   (action    (eql :filter-added)))
  ;; The default behavior for filter actions is to do nothing and
  ;; state the fact.
  :not-implemented)

(defmethod notify ((recipient t)
                   (subject   t)
                   (action    (eql :filter-removed)))
  ;; The default behavior for filter actions is to do nothing and
  ;; state the fact.
  :not-implemented)

;;; Error policy protocol

(defgeneric apply-error-policy (processor condition)
  (:documentation
   "Apply the error handling policy of PROCESSOR to CONDITION."))

;;; Configurator protocol

(defgeneric make-processor (configurator
                            &rest args
                            &key &allow-other-keys)
  (:documentation
   "Make and return a suitable processor instance for CONFIGURATOR.

    Methods of this generic function will usually call
    `collect-processor-mixins' and `ensure-processor-class' obtain the
    desired class of the processor being made. ARGS are passed to
    `make-instance' after the class has been determined."))

(defgeneric collect-processor-mixins (configurator)
  (:method-combination append)
  (:documentation
   "Return a list of names of mixin classes which should be combined
    to make the processor class for CONFIGURATOR."))
