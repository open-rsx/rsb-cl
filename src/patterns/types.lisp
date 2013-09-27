;;;; types.lisp --- Type definitions used the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.patterns)

(deftype method-name ()
  "Instances of this type are strings which are legal as method
names."
  'scope-component)

(deftype argument-style ()
  `(member :payload :event))

(deftype return-style ()
  "Instances of this type describe styles in which a result can be
returned from a method call."
  '(member :payload :event))

(deftype transform-specification ()
  "Instances of this type describe the transforms that should be
applied to the argument and return value of a method."
  `(or null
       (cons (cons (eql :argument) t) null)
       (cons (cons (eql :argument) t)
	     (cons (cons (eql :return)   t) null))
       (cons (cons (eql :return)   t) null)
       (cons (cons (eql :return) t)
	     (cons (cons (eql :argument)   t) null))))
