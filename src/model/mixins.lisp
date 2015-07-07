;;;; mixins.lisp --- Mixin classes used in the model module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model)

;;; `most-recent-activity-mixin'

(defclass most-recent-activity-mixin ()
  ((most-recent-activity :type     local-time:timestamp
                         :accessor info-most-recent-activity
                         :initform (local-time:now)
                         :documentation
                         "Stores the timestamp at which the most
                          recent activity involving the object
                          occurred."))
  (:documentation
   "This mixin stores a timestamp corresponding to the most recent
    activity on the object."))

(defun info-most-recent-activity-difference (thing
                                             &optional (time (local-time:now)))
  (local-time:timestamp-difference time (info-most-recent-activity thing)))

;;; `timing-info-mixin'

(defclass timing-info-mixin ()
  ((clock-offset :initarg  :clock-offset
                 :type     (or null real)
                 :accessor info-clock-offset
                 :initform nil
                 :documentation
                 "Stores the estimated difference in seconds between
                  the clock of the local host and the clock of the
                  remote host.")
   (latency      :initarg  :latency
                 :type     (or null real)
                 :accessor info-latency
                 :initform nil
                 :documentation
                 "Stores the mean event transport latency in seconds
                  between the local host and the remote host."))
  (:documentation
   "This mixin adds slots for storing an estimated clock offset and an
    estimated transport latency."))

;;; `info-mixin'

(defclass info-mixin ()
  ((info :initarg  :info
         :reader   node-info
         :documentation
         "Stores the `*-info' instance associated to the node."))
  (:default-initargs
   :info (missing-required-initarg 'info-mixin :info))
  (:documentation
   "This class is intended to be mixed into node classes that store an
    associated `*-info' instance."))

(defmethod print-items:print-items append ((object info-mixin))
  (print-items:print-items (node-info object)))

;;; Tree structure mixins

(defclass parented-mixin ()
  ((parent   :initarg  :parent
             :type     (or null node)
             :reader   node-parent
             :initform nil
             :documentation
             "Stores nil or the parent `node' instance of this node."))
  (:documentation
   "This class is intended to be mixed into node classes instances of
    which have an associated parent node."))

(defclass child-container-mixin ()
  ((children :initarg  :children
             :type     list #| of node |#
             :accessor node-children
             :initform '()
             :documentation
             "Stores a list of child `node' instances of this node"))
  (:documentation
   "This class is intended to be mixed into node classes instances of
    which have a list of child nodes."))
