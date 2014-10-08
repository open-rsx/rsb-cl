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

(defun uri-transport (uri)
  "If URI specifies a transport configuration, return three values:
the transport name, hostname and port. Both hostname and port can be
nil. If URI does not specify a transport, return nil."
  (when (puri:uri-scheme uri)
    (values (puri:uri-scheme uri) (puri:uri-host uri) (puri:uri-port uri))))

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
                (warn "~@<Ignoring query-option ~S - use ~:*~(~A~) part ~
                       of the URI instead.~@:>"
                      key)
                (appending (list key value)))))))

(defun uri->scope-and-options (uri &optional defaults)
  "Dissect URI of the form

  SCHEME:[//HOST][:PORT][/PATH][?QUERY][#FRAGMENT]

as follows:

+ SCHEME   -> Transport name
+ HOST     -> Transport option :HOST
+ PORT     -> Transport option :PORT
+ PATH     -> Scope
+ QUERY    -> \"freestyle\" transport options. Has to be of the form
              KEY1=VALUE1;KEY2=VALUE2;...
+ FRAGMENT -> not processed

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
  (let+ (((&values transport host port) (uri-transport uri))
         ((&accessors-r/o (path        puri:uri-path)
                          (fragment    puri:uri-fragment)
                          (uri-options uri-options)) uri)
         (transport-options
          (%transport-options transport defaults host port)))
    (when fragment
      (warn "~@<Ignoring fragment ~S in URI -> scope and options ~
             translation. URI was ~S~@:>"
            fragment uri))
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
