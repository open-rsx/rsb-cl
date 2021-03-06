;;;; broadcast-processor.lisp --- Broadcast events to multiple handlers.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

;;; `broadcast-processor'

(defclass broadcast-processor ()
  ((handlers :initarg  :handlers
             :type     list
             :accessor handlers
             :initform '()
             :documentation
             "Stores a list of handlers to which events are dispatched
              via `handle'."))
  (:documentation
   "Instances maintain a list of handlers and dispatch events to these
    handlers.

    Methods on `handle' can be use to filter or transform
    events. Methods on `dispatch' can be used to modify dispatching
    behavior."))

(defmethod handle ((processor broadcast-processor)
                   (data      t))
  ;; Dispatch DATA to handlers of PROCESSOR in a manner defined by
  ;; methods on `dispatch'.
  (dispatch processor data))

(defmethod dispatch ((processor broadcast-processor)
                     (data      t))
  ;; Dispatch DATA to handlers of PROCESSOR.
  (handle (handlers processor) data))
