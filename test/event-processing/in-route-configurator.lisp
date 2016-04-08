;;;; in-route-configurator.lisp --- Unit tests for the in-route-configurator class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

(rsb.transport:register-transport
 :mock
 :schemas   :mock
 :wire-type 'string)

(defclass mock-connector (rsb.transport:connector
                          broadcast-processor)
  ()
  (:metaclass rsb.transport:connector-class)
  (:transport :mock)
  (:direction :in-pull)
  (:default-initargs
   :schema :mock))

(defun check-configurator
    (configurator scope direction configurator-filters processor-filters)
  (is (scope= (make-scope scope)   (configurator-scope configurator)))
  (is (eq     direction            (configurator-direction configurator)))
  (is (equal  configurator-filters (configurator-filters configurator)))
  (is (equal  processor-filters    (processor-filters
                                    (configurator-processor configurator)))))

(def-suite in-route-configurator-root
    :in event-processing-root
  :description
  "Root test suite for the `in-route-configurator-root' class.")
(in-suite in-route-configurator-root)

(test adding/removing-filters
  "Test the required state transitions and updates when adding and
   removing filters to an `in-route-configurator' instance."

  (let ((configurator (make-instance 'in-route-configurator
                                     :scope     "/foo/bar"
                                     :direction :in-pull))
        (filter       (make-instance 'rsb.filter:conjoin-filter)))
    ;; Adding and removing a filter should not have an effect when
    ;; there are no connectors.
    (notify configurator filter :filter-added)
    (check-configurator configurator "/foo/bar" :in-pull
                        (list filter) nil)

    (notify configurator filter :filter-removed)
    (check-configurator configurator "/foo/bar" :in-pull nil nil)

    ;; However, when there is a connector, the filter should get
    ;; propagated to the processor.
    (push (make-instance 'mock-connector)
          (configurator-connectors configurator))

    (notify configurator filter :filter-added)
    (check-configurator configurator "/foo/bar" :in-pull
                        (list filter) (list filter))

    (notify configurator filter :filter-removed)
    (check-configurator configurator "/foo/bar" :in-pull nil nil)))
