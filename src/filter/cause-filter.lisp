;;;; cause-filter.lisp --- Event filtering based on cause id.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass cause-filter (function-caching-mixin
                        funcallable-filter-mixin
                        print-items:print-items-mixin)
  ((cause :type     cause-pattern
          :reader   filter-cause
          :writer   (setf filter-%%cause)
          :documentation
          "Stores the cause to which the filter should restrict
           events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Discriminate based on the cause vector of events.

    Patterns for matching cause vector can be specified using the
    following initarg combinations:

    :cause T

      Matches events whose cause vector has at least one element.

    :cause EVENT-ID

      Matches events whose cause vector contains an element equal to
      the `event-id' EVENT-ID.

    :cause UUID

      Matches events whose cause vector contains an element equal to
      the `uuid' UUID.

    :cause STRING

      Matches events whose cause vector contains an element equal to
      the UUID of which STRING is the printed representation.

    :cause OCTET-VECTOR

      Matches events whose cause vector contains an element equal to
      the UUID consisting of the octets in OCTET-VECTOR.

    :origin (UUID | STRING | OCTET-VECTOR) :sequence-number NON-NEGATIVE-INTEGER

      Matches events whose cause vector contains an element equal to
      the event id described by the origin UUID or STRING or
      OCTET-VECTOR and the sequence number NON-NEGATIVE-INTEGER.

    :origin (UUID | STRING | OCTET-VECTOR)

      Matches events whose cause vector contains an element the origin
      part of which is equal to the UUID described by UUID, STRING or
      OCTET-VECTOR.

    :sequence-number NON-NEGATIVE-INTEGER

      Matches events whose cause vector contains an element the
      sequence number part of which is equal to
      NON-NEGATIVE-INTEGER."))

(service-provider:register-provider/class
 'filter :cause :class 'cause-filter)

(defmethod shared-initialize :before
    ((instance   cause-filter)
     (slot-names t)
     &key
     (cause           nil cause-supplied?)
     (origin          nil origin-supplied?)
     (sequence-number nil sequence-number-supplied?))
  (declare (ignore cause origin sequence-number))
  (cond
    ((not (or cause-supplied? origin-supplied? sequence-number-supplied?))
     (missing-required-initarg
      'cause-filter :cause-xor-origin-or-sequence-number))
    ((and cause-supplied? (or origin-supplied? sequence-number-supplied?))
     (incompatible-initargs 'cause-filter :cause :origin :sequence-number))))

(defmethod shared-initialize :after
    ((instance   cause-filter)
     (slot-names t)
     &key
     (cause           nil cause-supplied?)
     (origin          nil origin-supplied?)
     (sequence-number nil sequence-number-supplied?))
  (cond
    (cause-supplied?
     (setf (filter-%cause instance) cause))
    ((and origin-supplied? sequence-number-supplied?)
     (setf (filter-%cause instance) (cons origin sequence-number)))
    (origin-supplied?
     (setf (filter-%cause instance) (cons origin '*)))
    (sequence-number-supplied?
     (setf (filter-%cause instance) (cons '* sequence-number)))))

(defgeneric (setf filter-%cause) (new-value filter)
  (:method ((new-value (eql t)) (filter cause-filter))
    (setf (filter-%%cause filter) new-value))

  (:method ((new-value t) (filter cause-filter))
    (setf (filter-%%cause filter) (ensure-uuid new-value)))

  (:method ((new-value cons) (filter cause-filter))
    (check-type new-value (or event-id
                              (cons uuid-designator (eql *))
                              cause-pattern/sequence-number))
    (setf (filter-%%cause filter)
          (typecase new-value
            ((cons uuid-designator (eql *))
             (cons (ensure-uuid (car new-value)) (cdr new-value)))
            (t
             new-value)))))

(defmethod rsb.ep:access? ((processor cause-filter)
                           (part      (eql :causes))
                           (mode      (eql :read)))
  t)

(defmethod compute-filter-function ((filter cause-filter) &key next)
  (declare (ignore next))
  (let ((cause (filter-cause filter)))
    (etypecase cause
      ((eql t)
       (lambda (event)
         (when (event-causes event) t)))
      (event-id
       (lambda (event)
         (when (find cause (the list (event-causes event))
                     :test #'event-id=)
           t)))
      (uuid:uuid
       (lambda (event)
         (when (find cause (the list (event-causes event))
                     :key #'event-id->uuid :test #'uuid:uuid=)
           t)))
      (cause-pattern/origin
       (let ((origin (car cause)))
         (lambda (event)
           (when (find origin (the list (event-causes event))
                       :test #'uuid:uuid= :key #'car)
             t))))
      (cause-pattern/sequence-number
       (let ((sequence-number (cdr cause)))
         (declare (type sequence-number sequence-number))
         (lambda (event)
           (when (find sequence-number (the list (event-causes event))
                       :test #'= :key #'cdr)
             t)))))))

(defmethod print-items:print-items append ((object cause-filter))
  (let ((cause (filter-cause object)))
    (typecase cause
      ((or (eql t) uuid:uuid)
       `((:cause ,cause "~/rsb::print-id/")))
      (event-id
       `((:cause ,(list (car cause) (cdr cause)) "~{~/rsb::print-id/:~:D~}")))
      (cause-pattern/origin
       `((:cause ,(car cause) "~/rsb::print-id/:*")))
      (cause-pattern/sequence-number
       `((:cause ,(cdr cause) "*:~:D"))))))
