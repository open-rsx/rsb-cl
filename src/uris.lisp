;;;; uris.lisp --- URI-related functions used in rsb.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

(defun uri->scope-and-options (uri)
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

   Return two values: scope and transport options. If SCHEME is
   present, return transport options that enable only the transport
   designated by SCHEME and disable all other transports.

   Examples:
   RSB> (uri->scope-and-options (puri:parse-uri \"spread:\"))
   => (make-scope \"/\") '((:spread :enabled t &inherit) (t :enabled nil &inherit))
   :test #'equal

   RSB> (uri->scope-and-options (puri:parse-uri \"spread://localhost:4811\"))
   => (make-scope \"/\") '((:spread :enabled t :host \"localhost\" :port 4811) (t :enabled nil &inherit))
   :test #'equal

   RSB> (uri->scope-and-options (puri:uri \"/justscope\"))
   => (make-scope \"/justscope\") '()
   :test #'equal"
  (let+ (((&accessors-r/o (transport   puri:uri-scheme)
                          (host        puri:uri-host)
                          (port        puri:uri-port)
                          (path        puri:uri-path)
                          (fragment    puri:uri-fragment)
                          (uri-options uri-options))
          uri))
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
    ;; Return scope and transport options that disable all other
    ;; transports iff TRANSPORT is specified (via scheme component of
    ;; URI).
    (values (make-scope path)
            (append (when (or transport host  port uri-options)
                      `((,(or transport t)
                         ,@(when transport `(:enabled t))
                         ,@(when host      `(:host ,host))
                         ,@(when port      `(:port ,port))
                         ,@uri-options
                         &inherit)))
                    (when transport
                      '((t :enabled nil &inherit)))))))
