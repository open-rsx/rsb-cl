;;;; scope.lisp --- Unit tests for scope class and related functions.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite* scope-root
  :in root
  :description
  "Unit tests for the `scope' class.")
;; TODO (:setup
;;  (clrhash rsb::*scopes*))

(test derive-scope-component
  "Test function `derive-scope-component'."

  (mapcar (lambda+ ((input expected))
            (is (string= expected (derive-scope-component input))))
          '(("foo"  "foo")
            ("Foo"  "Foo")
            ("FOO"  "FOO")
            ("foO"  "foO")
            ("/foo" "_foo")
            ("foo/" "foo_")
            ("!@"   "__")
            ("_foo" "_foo")
            ("foo_" "foo_"))))

(test scope/construction/from-components
  "Test constructing `scope' instances from lists of component
   strings."

  (mapc (lambda+ ((input expected-components))
          (case expected-components
            (type-error (signals type-error (make-scope input)))
            (t          (is (equalp expected-components
                                    (scope-components (make-scope input)))))))
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
          (("BAR" "FOO")        ("BAR" "FOO")))))

(test scope/construction/from-string
  "Test constructing `scope' instances from strings."

  (mapc (lambda+ ((string expected-components))
          (case expected-components
            (scope-parse-error
             (signals scope-parse-error (make-scope string)))
            (t
             (is (equal expected-components
                        (scope-components (make-scope string)))))))
        '(;; Some invalid cases.
          ("foo"         scope-parse-error) ; missing leading "/"
          ("foo/bar"     scope-parse-error) ; likewise
          ("/foo "       scope-parse-error) ; invalid character: space
          ("/!@#/+1"     scope-parse-error) ; invalid characters: !@#+
          ("/foo/+1"     scope-parse-error) ; in second component
          (""            scope-parse-error) ; empty string

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
          ("/"           ()))))

(test scope/construction/invalid
  "Test attempting to construct `scope' instance from unsuitable
   objects."

  (mapc (lambda (input) (signals type-error (make-scope input)))
        '(5 t :foo #\a)))

(test scope/relations
  "Test relations between `scope' instances."

  (mapc (lambda+ ((left right relations equality))
          (let ((left      (make-scope left))
                (right     (make-scope right))
                (relations (ensure-list relations)))
            ;; Test sub/super relation.
            (when (member :sub relations)
              (is (sub-scope?   left right))
              (is (super-scope? right left)))
            (when (member :super relations)
              (is (super-scope? left right))
              (is (sub-scope?   right left)))
            (when (member :none relations)
              (is (not (sub-scope?   left right)))
              (is (not (super-scope? left right))))

            ;; Test equality relation.
            (ecase equality
              (=  (is (scope= left right)))
              (/= (is (not (scope= left right)))))))

        '(("/foo"     "/foo"     (:sub :super) =)
          ("/foo/"    "/foo"     (:sub :super) =)
          ("/foo/bar" "/foo"     :sub          /=)
          ("/foo/bar" "/foo/baz" :none         /=)
          ("/"        "/foo"     :super        /=)
          ("/bar"     "/baz"     :none         /=)
          ("/bar"     "/Bar"     :none         /=)
          ("/bar"     "/BAR"     :none         /=))))

(test merge-scopes
  "Test `merge-scopes' function."

  (mapc (lambda+ ((left right expected))
          (let ((result (merge-scopes left right)))
            (is (typep result 'scope))
            (if (eq expected +root-scope+)
                (is (eq     expected result))
                (is (scope= expected result)))))

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
          (("a")              ("b")              "/b/a"))))

(test enough-scope
  "Test `enough-scope' function."

  (mapc (lambda+ ((thing defaults expected))
          (case expected
            (error
             (signals error (enough-scope thing defaults)))
            (t
             (let ((result (enough-scope thing defaults)))
               (is (typep result 'scope))
               (if (eq expected +root-scope+)
                   (is (eq     expected result))
                   (is (scope= expected result)))))))

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
          ("/a/b"             "/a/b"        "/"))))

(test scope/intern
  "Test interning of `scope' instances."

  (mapc (lambda (scope)
          (let ((result-1 (make-scope scope :intern? t))
                (result-2 (make-scope scope :intern? t))
                (result-3 (make-scope scope)))
            (is (scope-interned? result-1))
            (is (scope-interned? result-2))
            ;; RESULT-3 can only be interned if SCOPE was of type `scope'
            ;; and was already interned.
            (is (eq (when (typep scope 'scope)
                      (scope-interned? scope))
                    (scope-interned? result-3)))

            (is (eq     result-1 result-2))
            (is (scope= result-1 result-3))))
        `("/"
          "/foo"
          ,(make-scope "/")
          ,(make-scope "/foo")
          ,(make-scope "/" :intern? t)
          ,(make-scope "/foo" :intern? t))))

(test scope/print
  "Test method on `print-object' for `scope' class."

  (mapc (lambda+ ((scope intern?))
          (check-print (make-scope scope :intern? intern?)))
        '(("/"         nil) ("/"         t)
          ("/foo"      nil) ("/foo"      t)
          ("/foo/bar/" nil) ("/foo/bar/" t)
          ("/Foo/BAR/" nil) ("/Foo/BAR/" t))))

(test intern-scope/stress
  "Stress test for the `intern-scope' function."

  (loop :repeat 1000
        :do (let+ ((table      (make-hash-table :test #'equal))
                   (components (list "a" "b")) ; TODO use a random list
                   (string     (format nil "~{/~A~}/" components))
                   ((&flet intern-one ()
                      (let* ((rsb::*scopes* table)
                             (scope         (intern-scope (make-scope components))))
                        (values scope (princ-to-string scope)))))
                   (interned (mapcar #'bt:join-thread
                                     (map-into (make-list 10)
                                               (curry #'bt:make-thread #'intern-one)))))
              (is (= 1 (hash-table-count table)))
              (is-true (every (compose (curry #'string= string) #'scope-string)
                              interned))
              (is-true (every (compose (curry #'equal components) #'scope-components)
                              interned))
              (is (= 1 (length (remove-duplicates interned :test #'eq)))))))
