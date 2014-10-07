;;;; uris.lisp --- URI-related functions used in cl-rsb.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defun parse-scope-or-uri (string)
  "PARSE string as either a URI or a scope and return a `puri:uri' or
   `scope' instance respectively."
  (if (some (rcurry #'search string)
            '(":" "//" "#" "?"))
      (puri:parse-uri string)
      (make-scope string)))

(defun uri-options (uri)
  "Translate the query part of URI into a plist of options."
  (let+ (((&flet separator? (char)
            (or (eq char #\;) (eq char #\&))))
         (queries (split-sequence-if
                   #'separator? (puri:uri-query uri)
                   :remove-empty-subseqs t))
         (names-and-values (mapcar (curry #'split-sequence #\=)
                                   queries)))
    (iter (for (name value) in names-and-values)
          (let ((key (make-keyword (string-upcase name))))
            (if (member key '(:scheme :host :port) :test #'eq)
                (error "~@<Illegal query option \"~(~A~)\". ~:*~@(~A~) ~
                        URI component must be used instead.~@:>"
                        key)
                (appending (list key value)))))))

(defun uri->scope-and-options (uri &optional defaults)
  "Dissect URI of the form

     [SCHEME:[//HOST[:PORT]]][PATH][?QUERY][#FRAGMENT]

   as follows:

     SCHEME   -> Transport name
     HOST     -> Transport option :HOST
     PORT     -> Transport option :PORT
     PATH     -> Scope
     QUERY    -> \"freestyle\" transport options. Has to be of the form

                   KEY1=VALUE1;KEY2=VALUE2;...

     FRAGMENT -> not allowed

   Return two values: scope and transport options.

   Examples:
   RSB> (uri->scope-and-options (puri:parse-uri \"spread:\"))
   => (make-scope \"/\") '((:spread))
   :test #'equal

   RSB> (uri->scope-and-options (puri:parse-uri \"spread://localhost:4811\"))
   => (make-scope \"/\") '((:spread :port 4811 :host \"localhost\"))
   :test #'equal

   RSB> (uri->scope-and-options (puri:parse-uri \"spread:\") '((:spread :port 4811)))
   => (make-scope \"/\") '((:spread :port 4811))
   :test #'equal"
  (let+ (((&accessors-r/o (transport   puri:uri-scheme)
                          (host        puri:uri-host)
                          (port        puri:uri-port)
                          (path        puri:uri-path)
                          (fragment    puri:uri-fragment)
                          (uri-options uri-options))
          uri)
         (transport-options
          (%transport-options transport defaults host port)))
    ;; Disallow host or port components without scheme.
    (when (and (or host port) (not transport))
      (error "~@<URI ~S has ~{~{~A component ~S~}~^ and ~} but no ~
              scheme component.~@:>"
             (princ-to-string uri)
             (append (when host `(("host" ,host)))
                     (when port `(("port" ,port))))))
    ;; Disallow fragment component.
    (when fragment
      (error "~@<URI ~A has fragment component ~S.~@:>"
             (princ-to-string uri) fragment))
    (values (make-scope path)
            (mapcar (curry #'%merge-options uri-options)
                    transport-options))))

;;; Utility functions

(defun %transport-options (transport defaults host port)
  "Extract options for TRANSPORT from DEFAULTS if TRANSPORT is not
nil. If HOST and PORT are not nil, replace the host and port options
in the extracted transport options. Return the resulting options."
  (if transport
      (list (%merge-options
             (append (when host (list :host host))
                     (when port (list :port port))
                     (cdr (find t defaults :test #'eq :key #'car)))
             (or (find transport defaults :test #'eq :key #'car)
                 (list transport))))
      defaults))

(defun %merge-options (options transport-options)
  "Merge OPTIONS into TRANSPORT-OPTIONS such that options in OPTIONS
take precedence."
  (let+ (((transport &rest transport-options) transport-options))
    (cons transport (append options transport-options))))
