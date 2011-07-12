;;; uris.lisp --- URI-related functions used in cl-rsb.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb)

(defun uri-options (uri)
  "Translate the query part of URI into a plist of options."
  (bind (((:flet separator? (char))
	  (or (eq char #\;) (eq char #\&)))
	 (queries (split-sequence-if
		   #'separator? (puri:uri-query uri)
		   :remove-empty-subseqs t))
	 (names-and-values (map 'list (curry #'split-sequence #\=)
				queries)))
    (iter (for (name value) in names-and-values)
	  (let ((key (make-keyword (string-upcase name))))
	    (if (member key '(:host :port))
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
  (bind (((:accessors-r/o (transport   puri:uri-scheme)
			  (host        puri:uri-host)
			  (port        puri:uri-port)
			  (path        puri:uri-path)
			  (fragment    puri:uri-fragment)
			  (uri-options uri-options)) uri)
	 (transport-options
	  (%transport-options transport defaults host port)))
    (when (eq transport :rsb)
      (error "~@<~S schema is not supported yet.~@:>"
	     transport))
    (when fragment
      (warn "~@<Ignoring fragment ~S in URI -> scope and options translation. URI was ~S~@:>"
	    fragment uri))
    (values (make-scope path)
	    (map 'list (curry #'%merge-options uri-options)
		 transport-options))))


;;; Utility functions
;;

(defun %transport-options (transport defaults host port)
  "Extract options for TRANSPORT from DEFAULTS if TRANSPORT is not
nil. If HOST and PORT are not nil, replace the host and port options
in the extracted transport options. Return the resulting options."
  (if (and transport (not (eq transport :rsb)))
      (list (%merge-options
	     (append (when host (list :host host))
		     (when port (list :port port)))
	     (or (find transport defaults :key #'first)
		 (list transport))))
      defaults))

(defun %merge-options (options transport-options)
  "Merge OPTIONS into TRANSPORT-OPTIONS such that options in OPTIONS
take precedence."
  (bind (((transport &rest transport-options) transport-options))
    (cons transport (append options transport-options))))
