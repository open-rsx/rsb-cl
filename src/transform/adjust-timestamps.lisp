;;;; adjust-timestamps.lisp --- Transform that modifies event timestamps.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

;;; Timestamp adjustment specifications

(deftype timestamp-adjustment-value/self ()
  "Indicates that the current time (i.e. \"now\", time of processing
   the event) should replace the stored timestamp."
  '(eql :self))

(deftype timestamp-adjustment-value/now ()
  "Indicates that the current time (i.e. \"now\", time of processing
   the event) should replace the stored timestamp."
  '(eql :now))

(deftype timestamp-adjustment-value/copy ()
  "A value of the form

     (:COPY TIMESTAMP-DESIGNATOR)

   indicates that the timestamp designated by TIMESTAMP-DESIGNATOR
   should be extracted from the event and used to replace the stored
   timestamp."
  '(cons (eql :copy) (cons timestamp-designator null)))

(deftype timestamp-adjustment-value/adjust ()
  "A value of the form

     (+ TIMESTAMP-ADJUSTMENT-VALUE AMOUNT)

   indicates that the timestamp should be adjusted by AMOUNT seconds."
  '(cons (eql +) (cons t (cons real null))))

(deftype timestamp-adjustment-value ()
  "Specification of a replacement value for a particular timestamp."
  '(or local-time:timestamp
       timestamp-designator
       timestamp-adjustment-value/self
       timestamp-adjustment-value/now
       timestamp-adjustment-value/copy
       timestamp-adjustment-value/adjust))

(deftype timestamp-adjustment-spec ()
  "Replacement rule of the form

     (TIMESTAMP-DESIGNATOR TIMESTAMP-ADJUSTMENT-VALUE)

   specifying that the timestamp designated by TIMESTAMP-DESIGNATOR
   should be replaced with the timestamp value specified by
   TIMESTAMP-ADJUSTMENT-VALUE"
  '(cons timestamp-designator (cons timestamp-adjustment-value null)))

;;; Utility functions

(defun adjust-timestamps! (adjustments event)
  (labels ((adjust (key spec)
             (etypecase spec
               (local-time:timestamp
                (local-time::clone-timestamp spec))

               (timestamp-adjustment-value/self
                (adjust key key))

               (timestamp-adjustment-value/now
                (local-time:now))

               (timestamp-designator
                (local-time::clone-timestamp
                 (or (timestamp event spec)
                     (error "~@<Event ~A does not have a ~A ~
                             timestamp.~:@>"
                            event spec))))

               (timestamp-adjustment-value/copy
                (adjust key (second spec)))

               (timestamp-adjustment-value/adjust
                (let+ (((base adjustment) (rest spec))
                       ((&values sec nsec) (floor adjustment)))
                  (local-time:adjust-timestamp (adjust key base)
                    (:offset :sec  sec)
                    (:offset :nsec (floor (* 1000000000 nsec)))))))))
    (declare (dynamic-extent #'adjust))
    (iter (for (key spec) in adjustments)
          (setf (timestamp event key) (adjust key spec))))
  event)

;;; `adjust-timestamps' transform

(defclass adjust-timestamps (funcallable-transform-mixin
                             print-items:print-items-mixin)
  ((adjustments :type     list
                :accessor transform-adjustments
                :initform '()
                :documentation
                "Stores a list of adjustments of the form

                   (TIMESTAMP NEW-VALUE)

                 where TIMESTAMP is a keyword designating a timestamp
                 and NEW-VALUE is a specification of type
                 `timestamp-adjustment-value' for the new value."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Adjusts event timestamps according to given rules.

    Rules can refer to existing timestamps and the current time and
    include simple computations."))

(service-provider:register-provider/class
 'transform :adjust-timestamps :class 'adjust-timestamps)

(defmethod shared-initialize :after ((instance   adjust-timestamps)
                                     (slot-names t)
                                     &key
                                     (adjustments '() adjustments-supplied?))
  (when adjustments-supplied?
    (setf (transform-adjustments instance) adjustments)))

(defmethod (setf transform-adjustments) :before ((new-value t)
                                                 (object    adjust-timestamps))
  (unless (and (listp new-value)
               (every (of-type 'timestamp-adjustment-spec) new-value))
    (error 'type-error
           :datum         new-value
           :expected-type '(or null (cons timestamp-adjustment-spec)))))

(defmethod rsb.ep:access? ((transform adjust-timestamps)
                           (part      (eql :timestamp))
                           (mode      t))
  t)

(defmethod transform! ((transform adjust-timestamps) (event event))
  (adjust-timestamps! (transform-adjustments transform) event))

(defmethod print-items:print-items append ((object adjust-timestamps))
  `((:adjustments ,(transform-adjustments object) "~{~{~A ← ~A~}~^, ~}")))
