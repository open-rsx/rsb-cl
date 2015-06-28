;;;; scope.lisp --- Unit tests for scope class and related functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
        ("Foo"  "Foo")
        ("FOO"  "FOO")
        ("foO"  "foO")
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
  construction/from-components

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
        (("foo" "-")          ("foo" "-"))
        (("Bar" "Foo")        ("Bar" "Foo"))
        (("BAR" "FOO")        ("BAR" "FOO")))
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
        ("foo"         type-error) ; missing leading "/"
        ("foo/bar"     type-error) ; likewise
        ("/foo "       type-error) ; invalid character: space
        ("/!@#/+1"     type-error) ; invalid characters: !@#+
        ("/foo/+1"     type-error) ; in second component
        (""            type-error) ; empty string

        ;; These are valid.
        ("///Foo//BAR" ("Foo" "BAR"))
        ("//foo/bar"   ("foo" "bar"))
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
          "Test attempting to construct `scope' instance from
           unsuitable objects.")
  construction/invalid

  (ensure-cases (input)
      '(5 t :foo #\a)
    (ensure-condition 'type-error (make-scope input))))

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
        ("/bar"     "/baz"     :none         /=)
        ("/bar"     "/Bar"     :none         /=)
        ("/bar"     "/BAR"     :none         /=))

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
          "Test `merge-scopes' function.")
  merge-scopes

  (ensure-cases (left right expected)
      `((,+root-scope+      ,+root-scope+      ,+root-scope+)
        ("/"                ,+root-scope+      "/")
        (()                 ,+root-scope+      "/")
        (,+root-scope+      "/"                "/")
        (,+root-scope+      ()                 "/")

        (,(make-scope "/a") ,+root-scope+      "/a")
        ("/a"               ,+root-scope+      "/a")
        (("a")              ,+root-scope+      "/a")
        (,(make-scope "/a") "/"                "/a")
        ("/a"               "/"                "/a")
        (("a")              "/"                "/a")
        (,(make-scope "/a") ()                 "/a")
        ("/a"               ()                 "/a")
        (("a")              ()                 "/a")

        (,+root-scope+      ,(make-scope "/a") "/a")
        (,+root-scope+      "/a"               "/a")
        (,+root-scope+      ("a")              "/a")
        ("/"                ,(make-scope "/a") "/a")
        ("/"                "/a"               "/a")
        ("/"                ("a")              "/a")
        (()                 ,(make-scope "/a") "/a")
        (()                 "/a"               "/a")
        (()                 ("a")              "/a")

        (,(make-scope "/a") ,(make-scope "/b") "/b/a")
        ("/a"               ,(make-scope "/b") "/b/a")
        (("a")              ,(make-scope "/b") "/b/a")
        (,(make-scope "/a") "/b"               "/b/a")
        ("/a"               "/b"               "/b/a")
        (("a")              "/b"               "/b/a")
        (,(make-scope "/a") ("b")              "/b/a")
        ("/a"               ("b")              "/b/a")
        (("a")              ("b")              "/b/a"))

    (let ((result (merge-scopes left right)))
      (ensure (typep result 'scope))
      (ensure-same result expected
                   :test (if (eq expected +root-scope+) #'eq #'scope=)))))

(addtest (scope-root
          :documentation
          "Test `enough-scope' function.")
  enough-scope

  (ensure-cases (thing defaults expected)
      `(;; Test coercion.
        (,+root-scope+      ,+root-scope+ ,+root-scope+)
        ("/"                ,+root-scope+ "/")
        (()                 ,+root-scope+ "/")
        (,+root-scope+      "/"           "/")
        (,+root-scope+      ()            "/")

        (,(make-scope "/a") ,+root-scope+ "/a")
        ("/a"               ,+root-scope+ "/a")
        (("a")              ,+root-scope+ "/a")
        (,(make-scope "/a") "/"           "/a")
        ("/a"               "/"           "/a")
        (("a")              "/"           "/a")
        (,(make-scope "/a") ()            "/a")
        ("/a"               ()            "/a")
        (("a")              ()            "/a")

        ;; Test semantics.
        ("/"                "/"           ,+root-scope+)
        ("/a"               "/"           "/a")
        ("/a/b"             "/"           "/a/b")

        ("/"                "/a"          error)
        ("/a"               "/a"          ,+root-scope+)
        ("/a/b"             "/a"          "/b")

        ("/"                "/a/b"        error)
        ("/a"               "/a/b"        error)
        ("/a/b"             "/a/b"        "/"))

    (case expected
      (error
       (ensure-condition error (enough-scope thing defaults)))
      (t
       (let ((result (enough-scope thing defaults)))
         (ensure (typep result 'scope))
         (ensure-same result expected
                      :test (if (eq expected +root-scope+) #'eq #'scope=)))))))

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
        ("/foo/bar/" nil) ("/foo/bar/" t)
        ("/Foo/BAR/" nil) ("/Foo/BAR/" t))
   (check-print (make-scope scope :intern? intern?))))

(addtest (scope-root
          :documentation
          "Stress test for the `intern-scope' function.")
  intern-scope/stress

  (loop :repeat 1000 :do
     (let+ ((table      (make-hash-table :test #'equal))
            (components (list "a" "b")) ; TODO use a random list
            (string     (format nil "~{/~A~}/" components))
            ((&flet intern-one ()
               (let* ((rsb::*scopes* table)
                      (scope         (intern-scope (make-scope components))))
                 (values scope (princ-to-string scope)))))
            (interned (mapcar #'bt:join-thread
                              (map-into (make-list 10)
                                        (curry #'bt:make-thread #'intern-one)))))
       (ensure-same (hash-table-count table) 1)
       (ensure (every (compose (curry #'string= string) #'scope-string)
                      interned))
       (ensure (every (compose (curry #'equal components) #'scope-components)
                      interned))
       (ensure-same (length (remove-duplicates interned :test #'eq)) 1))))
