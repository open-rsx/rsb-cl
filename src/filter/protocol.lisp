;;;; protocol.lisp --- Protocol for event filtering.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Filter protocol

(defgeneric matches? (filter event)
  (:documentation
   "Return non-nil if EVENT matches the criteria of FILTER."))

;; Default behavior

(defmethod matches? ((filter function) (event t))
  "Apply FILTER to EVENT."
  (funcall filter event))

;;; Payload matching protocol

(defgeneric payload-matches? (filter payload)
  (:documentation
   "Return non-nil if PAYLOAD matches the criteria of FILTER."))

;;; Filter function protocol

(defgeneric filter-function (filter)
  (:documentation
   "Return the effective filter function of FILTER.

    The returned function takes an event as its sole argument and
    returns a Boolean indicating whether the event matches the
    criteria of FILTER."))

(defgeneric compute-filter-function (filter &key next)
  (:documentation
   "Compute and return an effective filter function for FILTER.

    The returned function takes an event as its sole argument and
    returns a Boolean indicating whether the event matches the
    criteria of FILTER.

    The computed function can call the function NEXT in case it cannot
    decide whether the event matches the criteria of FILTER."))

;;; Filter service

(service-provider:define-service filter
  (:documentation
   "Providers of this service implement event filtering strategies by
    via methods on the `matches?' generic function.

    Filter instances should also be funcallable."))

(defun make-filter (name &rest args)
  "Construct an instance of the filter class designated by NAME using
   ARGS as initargs."
  (apply #'service-provider:make-provider 'filter name args))

;;; Filter construction mini-DSL

(defgeneric filter (spec
                    &rest args
                    &key &allow-other-keys)
  (:documentation
   "Construct and return a filter instance according to SPEC and
ARGS. SPEC is either a keyword designating a filter class or a list of
the form

  (CLASS (CHILDSPEC1) (CHILDSPEC2) ...)

where CLASS designates a filter class and CHILDSPECN is of the same
form as SPEC. When this second form is used, CLASS has to designate a
composite filter class for which will `make-instance' will be called
with initargs consisting of ARGS and an additional :children
initarg. The value of this initarg is computed by recursively applying
`filter' to each CHILDSPECN."))

(defmethod filter ((spec symbol)
                   &rest args
                   &key)
  (handler-bind
      ((error (lambda (condition)
                (error 'filter-construction-error
                       :spec  (cons spec args)
                       :cause condition))))
    (apply #'make-filter spec args)))

(defmethod filter ((spec list)
                   &rest args
                   &key)
  (let+ (((class &rest child-specs) spec)
         (children (map 'list (curry  #'apply #'filter) child-specs)))
    (apply #'filter class :children children args)))
