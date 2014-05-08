;;;; reader.lisp --- Pull-based receiving participant class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass reader (receiving-client)
  ((direction :allocation :class
              :initform :in-pull))
  (:documentation
   "Instances of this class provide a pull-based interface for
receiving events."))

(defmethod receive ((reader reader)
                    &key
                    (block? t))
  (let ((processor (rsb.ep:configurator-processor
                    (rsb.ep:client-configurator reader))))
    (rsb.ep:emit processor block?)))

;;; `reader' creation

(defmethod make-reader ((scope scope)
                        &rest args &key
                        (transports (transport-options))
                        (converters (default-converters))
                        transform
                        error-policy)
  (declare (ignore transform error-policy))
  ;; Translate different kinds of errors into `reader-creation-error'
  ;; errors.
  (with-condition-translation
      (((error reader-creation-error)
        :kind       :reader
        :scope      scope
        :transports transports))
    (apply #'make-participant 'reader scope
           :transports transports
           :converters converters
           args)))

(define-participant-creation-uri-methods reader (scope puri:uri))

(define-participant-creation-restart-method reader (scope scope))
(define-participant-creation-restart-method reader (scope puri:uri))
