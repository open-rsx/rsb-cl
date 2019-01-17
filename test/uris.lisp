;;;; uris.lisp --- Unit tests for URI-related functions.
;;;;
;;;; Copyright (C) 2011-2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite* uris-root
  :in root
  :description
  "Unit tests for URI-related functions.")

(test uri-options-smoke
  "Smoke test for the `uri-options' function."

  (mapc (lambda+ ((uri-string expected))
          (let+ (((&flet do-it ()
                    (let ((uri (puri:parse-uri uri-string)))
                      (values (uri-options uri) uri)))))
            (case expected
              (error
               (signals error (do-it)))
              (t
               (let+ (((&values options uri) (do-it)))
                 (is (equal expected options)
                     "~@<Extracted options from URI ~S are ~S, not ~
                      ~S.~@:>"
                     uri options expected))))))

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
          ("?first=yes&amp=yes"    (:first "yes" :amp "yes")))))

(test uri->scope-and-options-smoke
  "Smoke test for the `scope->uri-and-options' function."

  (mapc
   (lambda+ ((uri-string expected-scope &optional expected-options))
     (let+ (((&flet do-it ()
               (uri->scope-and-options (puri:parse-uri uri-string)))))
       (case expected-scope
         (error
          (signals error (do-it)))
         (t
          (let+ (((&values scope options) (do-it)))
            (is (scope= expected-scope scope))
            (is (equalp expected-options options)))))))

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
     ("?port=4444"                    error))))
