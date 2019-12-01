;;;; prefix-scope.lisp --- Unit tests for the prefix-scope transform.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(def-suite* rsb.transform.prefix-scope-root
  :in rsb.transform-root
  :description
  "Unit tests for the `prefix-scope' transform.")

(test prefix-scope.construct
  "Test constructing `prefix-scope' instances."

  (signals transform-creation-error (make-transform :prefix-scope))
  (signals transform-creation-error (make-transform :prefix-scope :prefix 1)))

(test prefix-scope.smoke
  "Smoke test for the `prefix-scope' transform."

  (mapc
   (lambda+ ((prefix event-scope expected))
     (call-with-transform-checking-thunk
      (lambda (do-it)
        (is (scope= expected (event-scope (funcall do-it)))))
      (list :prefix-scope :prefix prefix)
      (list event-scope "")))

   '(("/"        "/"        "/")
     ("/"        "/foo"     "/foo")
     ("/"        "/foo/bar" "/foo/bar")
     ("/baz"     "/"        "/baz/")
     ("/baz"     "/foo"     "/baz/foo")
     ("/baz"     "/foo/bar" "/baz/foo/bar")
     ("/baz/fez" "/"        "/baz/fez/")
     ("/baz/fez" "/foo"     "/baz/fez/foo")
     ("/baz/fez" "/foo/bar" "/baz/fez/foo/bar"))))

(test prefix-scope.print
  "Test printing `prefix-scope' instances."

  (let ((transform (make-transform :prefix-scope :prefix "/foo")))
    (is (search "/foo" (princ-to-string transform)))))
