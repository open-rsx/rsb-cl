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

  (ensure-cases (uri-string expected-scope &optional expected-options)
      '(;; From https://code.cor-lab.de/projects/rsb/wiki/URI_Schema
        (""                              "/"        ())
        ("spread:"                       "/"        ((:spread    :enabled t                                &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("inprocess:"                    "/"        ((:inprocess :enabled t                                &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("spread://localhost:5555"       "/"        ((:spread    :enabled t   :host "localhost" :port 5555 &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("inprocess://someotherhost"     "/"        ((:inprocess :enabled t   :host "someotherhost"        &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("spread:/foo/bar"               "/foo/bar" ((:spread    :enabled t                                &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("spread:?maxfragmentsize=10000" "/"        ((:spread    :enabled t   :maxfragmentsize "10000"     &inherit)
                                                     (t          :enabled nil                              &inherit)))

        ;; Additional
        ("foo:/bla?bar=baz;awesome=no"   "/bla"     ((:foo       :enabled t   :bar "baz" :awesome "no"     &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("bar:"                          "/"        ((:bar       :enabled t                                &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("bar://baz:20"                  "/"        ((:bar       :enabled t   :host "baz" :port 20         &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("bar://baz"                     "/"        ((:bar       :enabled t   :host "baz"                  &inherit)
                                                     (t          :enabled nil                              &inherit)))
        ("/?foo=5&bar=whoop"             "/"        ((t                       :foo "5" :bar "whoop"        &inherit)))

        ;; Some illegal cases.
        ("#fragment"                     error)
        ("//host"                        error)
        (":1234"                         error)
        ("//host:1234"                   error)
        ("?host=foo"                     error)
        ("?port=4444"                    error))

    (let+ (((&flet do-it ()
              (uri->scope-and-options (puri:parse-uri uri-string)))))
      (case expected-scope
        (error
         (ensure-condition 'error (do-it)))
        (t
         (let+ (((&values scope options) (do-it)))
           (ensure-same scope expected-scope
                        :test      #'scope=
                        :report    "~@<Expected scope ~S, not ~S.~@:>"
                        :arguments (expected-scope scope))
           (ensure-same options expected-options
                        :test      #'equalp
                        :report    "~@<Expected options ~S, not ~S.~@:>"
                        :arguments (expected-options options))))))))
