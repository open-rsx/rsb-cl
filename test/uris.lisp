;;;; uris.lisp --- Unit tests for URI-related functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite uris-root (root)
  ()
  (:documentation
   "Unit tests for URI-related functions."))

(addtest (uris-root
          :documentation
          "Smoke test for the `uri-options' function.")
  uri-options-smoke

  (ensure-cases (uri-string expected)
      '(;; Some illegal cases.
        ("?scheme=foo"           error)
        ("?host=foo"             error)
        ("?port=4444"            error)
        ;; These are allowed.
        (""                      ())
        ("spread:"               ())
        ("?"                     ())
        ("?foo=bar"              (:foo "bar"))
        ("?bar=baz;whoop=nooble" (:bar "baz" :whoop "nooble"))
        ("?first=yes&amp=yes"    (:first "yes" :amp "yes")))

    (let+ (((&flet do-it ()
              (let ((uri (puri:parse-uri uri-string)))
                (values (uri-options uri) uri)))))
      (case expected
        (error
         (ensure-condition 'error (do-it)))
        (t
         (let+ (((&values options uri) (do-it)))
           (ensure-same options expected
                        :test      #'equal
                        :report    "~@<Extracted options from URI ~S are ~
                                    ~S, not ~S.~@:>"
                        :arguments (uri options expected))))))))

(addtest (uris-root
          :documentation
          "Smoke test for the `scope->uri-and-options' function.")
  uri->scope-and-options-smoke

  (ensure-cases (uri-string defaults expected-scope expected-options)
      '(;; From https://code.cor-lab.de/projects/rsb/wiki/URI_Schema
        (""                              nil "/"        nil)
        ("spread:"                       nil "/"        ((:spread)))
        ("inprocess:"                    nil "/"        ((:inprocess)))
        ("spread://localhost:5555"       nil "/"        ((:spread :host "localhost" :port 5555)))
        ("inprocess://someotherhost"     nil "/"        ((:inprocess :host "someotherhost")))
        ("spread:/foo/bar"               nil "/foo/bar" ((:spread)))
        ("spread:?maxfragmentsize=10000" nil "/"        ((:spread :maxfragmentsize "10000")))
        ;; Additional
        ("foo:/bla?bar=baz;awesome=no"   nil "/bla"     ((:foo :bar "baz" :awesome "no")))
        ("bar:" ((:bar :whoop 10)) "/" ((:bar :whoop 10)))
        ("bar://baz:20" ((:bar :port 10)) "/" ((:bar :host "baz" :port 20 :port 10)))
        ("bar://baz" ((:bar :port 10)) "/" ((:bar :host "baz" :port 10)))
        ("/?foo=5&bar=whoop"
         ((:spread :port 10 :bar "baz") (:inprocess :bla 5))
         "/"
         ((:spread :foo "5" :bar "whoop" :port 10 :bar "baz")
          (:inprocess :foo "5" :bar "whoop" :bla 5))))

    (let+ ((uri (puri:parse-uri uri-string))
           ((&values scope options)
            (uri->scope-and-options uri defaults)))
      (ensure-same scope expected-scope
                   :test      #'scope=
                   :report    "~@<Expected scope ~S, not ~S.~@:>"
                   :arguments (expected-scope scope))
      (ensure-same options expected-options
                   :test      #'equalp
                   :report    "~@<Expected options ~S, not ~S.~@:>"
                   :arguments (expected-options options)))))
