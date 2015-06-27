;;;; prefix-scope.lisp --- Unit tests for the prefix-scope transform.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(deftestsuite rsb.transform.prefix-scope-root (rsb.transform-root)
  ()
  (:documentation
   "Unit tests for the `prefix-scope' transform."))

(addtest (rsb.transform.prefix-scope-root
          :documentation
          "Test constructing `prefix-scope' instances.")
  construct

  (ensure-condition transform-creation-error
    (make-transform :prefix-scope))
  (ensure-condition transform-creation-error
    (make-transform :prefix-scope :prefix 1)))

(addtest (rsb.transform.prefix-scope-root
          :documentation
          "Smoke test for the `prefix-scope' transform.")
  smoke

  (ensure-cases (prefix event-scope expected)
      '(("/"        "/"        "/")
        ("/"        "/foo"     "/foo")
        ("/"        "/foo/bar" "/foo/bar")
        ("/baz"     "/"        "/baz/")
        ("/baz"     "/foo"     "/baz/foo")
        ("/baz"     "/foo/bar" "/baz/foo/bar")
        ("/baz/fez" "/"        "/baz/fez/")
        ("/baz/fez" "/foo"     "/baz/fez/foo")
        ("/baz/fez" "/foo/bar" "/baz/fez/foo/bar"))

    (let* ((event     (make-event event-scope ""))
           (transform (make-transform :prefix-scope :prefix prefix))
           (result    (transform! transform event)))
      (ensure-same expected (event-scope result) :test #'scope=))))

(addtest (rsb.transform.prefix-scope-root
          :documentation
          "Test printing `prefix-scope' instances.")
  print

  (let ((transform (make-transform :prefix-scope :prefix "/foo")))
    (ensure (search "/foo" (princ-to-string transform)))))
