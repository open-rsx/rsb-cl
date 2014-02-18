;;;; reader.lisp --- Pull-based receiving participant class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass reader (receiving-client)
  ()
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
                        &key
                        (transports (transport-options))
                        (converters (default-converters))
                        transform)
  ;; Translate different kinds of errors into `reader-creation-error'
  ;; errors.
  (with-condition-translation
      (((error reader-creation-error)
        :scope      scope
        :transports transports))
    (make-participant 'reader scope :in-pull
                      transports converters transform)))

(define-participant-creation-uri-methods reader (scope puri:uri))

(define-participant-creation-restart-method reader (scope scope))
(define-participant-creation-restart-method reader (scope puri:uri))
