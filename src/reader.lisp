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
   "Called by client to receive events on a channel (pull style).

    The client makes blocking or non-blocking calls to `receive' to
    receive the next event."))

(register-participant-class 'reader)

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
  (apply #'make-participant :reader scope
         :transports transports
         :converters converters
         args))
