;;;; out-connector.lisp --- An out-direction connector for inprocess communication.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-out)))
  (find-class 'out-connector))

(defclass out-connector (connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod handle :before ((connector out-connector)
                           (event     event))
  (setf (timestamp event :send) (local-time:now)))

(defmethod handle ((connector out-connector) (event event))
  (iter (for super in (super-scopes (event-scope event)
                                    :include-self? t))
        (handle (by-scope super) event)))
