;;;; package.lisp --- Package definition filter module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

  ;; Filter function protocol
  (:export
   #:filter-function
   #:compute-filter-function)

  ;; Filter construction
  (:export
   #:make-filter)

  ;; `function-caching-mixin' class
  (:export
   #:function-caching-mixin
   #:update-filter-function)

  ;; `funcallable-filter-mixin' class
  (:export
   #:funcallable-filter-mixin)

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
   #:filter-scope
   #:filter-exact?)

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

  ;; `meta-data-filter' class
  (:export
   #:meta-data-filter
   #:filter-key
   #:filter-predicate)

  ;; `cause-filter' class
  (:export
   #:cause-filter
   #:filter-cause)

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
