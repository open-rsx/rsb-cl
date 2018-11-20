;;;; reader.lisp --- Pull-based receiving participant class.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.reader)

(defclass reader (rsb::receiving-client)
  ((direction :allocation :class
              :initform :in-pull))
  (:documentation
   "Called by client to receive events on a channel (pull style).

    The client makes blocking or non-blocking calls to `receive' to
    receive the next event."))

(rsb::register-participant-class 'reader)

(defmethod receive ((reader reader)
                    &key
                    (block? t))
  (let ((processor (rsb.ep:configurator-processor
                    (rsb.ep:client-configurator reader))))
    (rsb.ep:emit processor block?)))
