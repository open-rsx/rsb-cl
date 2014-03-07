;;;; package.lisp --- Package definition filter module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.filter
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; Conditions
  (:export
   #:filter-construction-error
   #:filter-construction-error-spec)

  ;; Filter protocol
  (:export
   #:matches?)

  ;; Filter class family and construction
  (:export
   #:no-such-filter
   #:find-filter-class
   #:filter-classes

   #:make-filter)

  ;; `filter-mixin' class
  (:export
   #:filter-mixin)

  ;; Composite filter protocol
  (:export
   #:composite-filter
   #:filter-children)

  ;; `conjoin-filter' class
  (:export
   #:conjoin-filter)

  ;; `complement-filter' class
  (:export
   #:complement-filter)

  ;; `disjoin-filter' class
  (:export
   #:disjoin-filter)

  ;; `fallback-policy-mixin' class
  (:export
   #:fallback-policy-mixin
   #:filter-fallback-policy)

  ;; `payload-matching-mixin' class
  (:export
   #:payload-matches?
   #:payload-matching-mixin)

  ;; `scope-filter' class
  (:export
   #:scope-filter
   #:filter-scope)

  ;; `type-filter' class
  (:export
   #:type-filter
   #:filter-type)

  ;; `origin-filter' class
  (:export
   #:origin-filter
   #:filter-origin)

  ;; `regex-filter' class
  (:export
   #:regex-filter
   #:filter-regex
   #:filter-case-sensitive?

   #:compile-regex)

  ;; `method-filter' class
  (:export
   #:method-filter
   #:filter-method)

  ;; `xpath-filter' class
  (:export
   #:xpath-filter
   #:filter-xpath
   #:filter-compiled-xpath)

  ;; DSL
  (:export
   #:filter)

  (:documentation
   "This package contains event filters. In general, filters are unary
predicates that discriminate arbitrary object and in particular
`rsb:event' instances. The filters in this package are implemented as
funcallable classes with a specialized `matches?' method as the
funcallable instance function."))
