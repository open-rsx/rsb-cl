;;;; reader.lisp --- Pull-based receiving participant class.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.reader)

(defclass reader (participant
                  composite-participant-mixin
                  child-container-mixin
                  configuration-inheritance-mixin)
  ((direction :allocation :class
              :initform :in-pull)
   (queue     :type     lparallel.queue:queue
              :reader   queue
              :initform (lparallel.queue:make-queue)
              :documentation
              "Stores events as they arrive via the message bus."))
  (:documentation
   "Called by client to receive events on a channel (pull style).

    The client makes blocking or non-blocking calls to `receive' to
    receive the next event."))

(rsb::register-participant-class 'reader)

(defmethod initialize-instance :after ((instance reader)
                                       &key
                                       (filters nil filters-supplied?))
  (setf (participant-child instance nil :listener)
        (apply #'make-child-participant
               instance nil :listener
               (when filters-supplied?
                 (list :filters filters)))))

(defmethod make-child-initargs ((participant reader)
                                (which       (eql nil))
                                (kind        (eql :listener))
                                &key)
  (let ((queue (queue participant)))
    (list* :handlers (list (lambda (event)
                             (lparallel.queue:push-queue event queue)))
           (call-next-method))))

(defmethod rsb:receiver-filters ((receiver reader))
  (rsb:receiver-filters (participant-child receiver nil :listener)))

(defmethod (setf rsb:receiver-filters) ((new-value list) (receiver reader))
  (setf (rsb:receiver-filters (participant-child receiver nil :listener))
        new-value))

(defmethod receive ((reader reader) &key (block? t))
  (let ((queue (queue reader)))
    (if block?
        (lparallel.queue:pop-queue queue)
        (lparallel.queue:try-pop-queue queue))))
