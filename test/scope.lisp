;;;; scope.lisp --- Unit tests for scope class and related functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite scope-root (root)
  ()
  (:setup
   (clrhash rsb::*scopes*))
  (:documentation
   "Unit tests for the `scope' class."))

(addtest (scope-root
          :documentation
          "Test function `derive-scope-component'.")
  derive-scope-component

  (ensure-cases (input expected)
      '(("foo"  "foo")
        ("/foo" "_foo")
        ("foo/" "foo_")
        ("!@"   "__")
        ("_foo" "_foo")
        ("foo_" "foo_"))
    (ensure-same (derive-scope-component input) expected :test #'string=)))

(addtest (scope-root
          :documentation
          "Test constructing `scope' instances from lists of component
strings.")
  construction/from-conponents

  (ensure-cases (input expected-components)
      `(;; Some invalid cases
        (("foo ")             type-error) ; invalid character: space
        (("!@#+1")            type-error) ; invalid characters: !@#+
        (("foo" "+1")         type-error) ; in second component
        (#("!@#" "foo")       type-error) ; likewise for vector
        (#("/")               type-error)
        (#("foo" "+1")        type-error)
        (("")                 type-error) ; empty component

        ;; These are valid
        (,(make-scope "/foo") ("foo"))
        (()                   ())
        (("bar" "foo")        ("bar" "foo"))
        (#("baz" "bar")       ("baz" "bar"))
        (("foo" "_")          ("foo" "_"))
        (("foo" "-")          ("foo" "-")))
    (case expected-components
      (type-error (ensure-condition 'type-error (make-scope input)))
      (t          (ensure-same (scope-components (make-scope input))
                               expected-components
                               :test #'equalp)))))

(addtest (scope-root
          :documentation
          "Test constructing `scope' instances from strings.")
  construction/from-string

  (ensure-cases (string expected-components)
      '(;; Some invalid cases.
        ("/foo "       type-error) ; invalid character: space
        ("/!@#/+1"     type-error) ; invalid characters: !@#+
        ("/foo/+1"     type-error) ; in second component
        (""            type-error) ; empty string

        ;; These are valid.
        ("///Foo//BAR" ("Foo" "BAR"))
        ("//foo/bar"   ("foo" "bar"))
        ("foo/bar"     ("foo" "bar"))
        ("//foo/bar"   ("foo" "bar"))
        ("/foo/bar/"   ("foo" "bar"))
        ("/foo/5/"     ("foo" "5"))
        ("/foo/-/"     ("foo" "-"))
        ("/foo/_/"     ("foo" "_"))
        ("/-/foo/"     ("-" "foo"))
        ("/_/foo/"     ("_" "foo"))
        ("/"           ()))
    (case expected-components
      (type-error (ensure-condition 'type-error (make-scope string)))
      (t          (ensure-same (scope-components (make-scope string))
                               expected-components
                               :test #'equal)))))

(addtest (scope-root
          :documentation
          "Test relations between `scope' instances.")
  relations

  (ensure-cases (left right relations equality)
      '(("/foo"     "/foo"     (:sub :super) =)
        ("/foo/"    "/foo"     (:sub :super) =)
        ("/foo/bar" "/foo"     :sub          /=)
        ("/foo/bar" "/foo/baz" :none         /=)
        ("/"        "/foo"     :super        /=)
        ("/bar"     "/baz"     :none         /=))

    (let ((left      (make-scope left))
          (right     (make-scope right))
          (relations (ensure-list relations)))
      ;; Test sub/super relation.
      (when (member :sub relations)
        (ensure (sub-scope?   left right))
        (ensure (super-scope? right left)))
      (when (member :super relations)
        (ensure (super-scope? left right))
        (ensure (sub-scope?   right left)))
      (when (member :none relations)
        (ensure (not (sub-scope?   left right)))
        (ensure (not (super-scope? left right))))

      ;; Test equality relation.
      (ecase equality
        (=  (ensure (scope= left right)))
        (/= (ensure (not (scope= left right))))))))

(addtest (scope-root
          :documentation
          "Test interning of `scope' instances.")
  intern

  (ensure-cases (scope)
      `("/"
        "/foo"
        ,(make-scope "/")
        ,(make-scope "/foo")
        ,(make-scope "/" :intern? t)
        ,(make-scope "/foo" :intern? t))
    (let ((result-1 (make-scope scope :intern? t))
          (result-2 (make-scope scope :intern? t))
          (result-3 (make-scope scope)))
      (ensure (scope-interned? result-1))
      (ensure (scope-interned? result-2))
      ;; RESULT-3 can only be interned if SCOPE was of type `scope'
      ;; and was already interned.
      (ensure (eq (scope-interned? result-3)
                  (when (typep scope 'scope)
                    (scope-interned? scope))))

      (ensure-same result-1 result-2 :test #'eq)
      (ensure-same result-1 result-3 :test #'scope=))))

(addtest (scope-root
          :documentation
          "Test method on `print-object' for `scope' class.")
  print

  (ensure-cases (scope intern?)
      '(("/"         nil) ("/"         t)
        ("/foo"      nil) ("/foo"      t)
        ("/foo/bar/" nil) ("/foo/bar/" t))
   (check-print (make-scope scope :intern? intern?))))
