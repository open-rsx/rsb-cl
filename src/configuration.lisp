;;; configuration.lisp ---
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


;;; Environment variables
;;

(defun options-from-environment ()
  "Obtain configuration options from environment variables."
  (bind (((:flet name->option-name (name))
	  (when (starts-with-subseq "RSB_" name)
	    (string->option-name (subseq name 4) #\_)))
	 ((:flet variable->option (string))
	  (bind (((name value) (split-sequence #\= string :count 2))
		 (name (name->option-name name)))
	    (when name
	      (cons name value)))))
    (effective-options
     (remove-if #'null (map 'list #'variable->option
			    (sb-impl::posix-environ))))))


;;; Configuration file
;;

(defun options-from-stream (stream)
  "Obtain configuration options from STREAM."
  (effective-options
   (iter (for  line     in-stream stream :using #'read-line)
	 (for  line-num :from     1)
	 (with section  =         nil)
	 (bind ((content (subseq line 0 (or (position #\# line)
					    (length line))))
		((:flet trim (string))
		 (string-trim '(#\Space) string)))
	   (cond
	     ;; Empty/comment-only line
	     ((emptyp content))

	     ;; Section header
	     ((and (starts-with #\[ content) (ends-with #\] content))
	      (setf section (string->option-name
			     (subseq content 1 (1- (length content))))))
	     ;; Value
	     ((= (funcall #'count #\= content) 1) ;; iterate :(
	      (bind (((name value)
		      (map 'list #'trim (split-sequence #\= content)))
		     (name (string->option-name name)))
		(collect (cons (append section name) value))))
	     ;; Invalid
	     (t
	      (error "~@<Syntax error in line ~D, contents ~S.~@:>"
		     line-num line)))))))


;;;
;;

(defun options-from-default-sources ()
  "Combine options from the following configuration sources:
+ ~/.config/rsb.conf
+ $(PWD)/rsb.conf
+ Environment Variables"
  (effective-options
   (append
    (options-from-environment)
    (with-input-from-file (stream "rsb.conf"
				  :if-does-not-exist nil)
      (when stream (options-from-stream stream)))
    (with-input-from-file (stream "~/.config/rsb.conf"
				  :if-does-not-exist nil)
      (when stream (options-from-stream stream)))
    '(((:transport :spread :converter)
       . (:fundamental-utf-8-string :fundamental-bytes))))))


;;;
;;

;; Forward declaration of `*default-configuration*'
(defvar *default-configuration*)

(defun section-options (section &optional
			(config *default-configuration*))
  (bind ((section (ensure-list section))
	 ((:flet strip-key (option))
	  (cons (nthcdr (length section) (car option))
		(cdr option))))
    (map 'list #'strip-key
	 (remove section config
		 :test-not #'starts-with-subseq
		 :key      #'car))))

(defun option-value (name &optional default
		     (config *default-configuration*))
  "DOC"
  (let ((option (assoc name config :test #'equal)))
    (if option
	(cdr option)
	default)))


;;;
;;

(defun transport-options (&optional (config *default-configuration*))
  "Collect and interpret options in CONFIG that apply to
transports. Options for transports which are disabled in CONFIG are
not returned."
  (bind (((:flet options->plist (options))
	  (iter (for (key . value) in options)
		(when (length= 1 key)
		  (collect (first key)) (collect value))))
	 (options    (section-options :transport config))
	 (transports (remove-duplicates
		      (map 'list (compose #'first #'car) options))))
    ;; Collect options for all individual transport. Skip disabled
    ;; transports.
    (iter (for transport in transports)
	  (bind ((options (section-options transport options)))
	    (when (option-value :enabled t options)
	      (collect (cons transport (options->plist options))))))))


;;; Utility functions
;;

(defun string->option-name (string &optional (separator #\.))
  "DOC"
  (map 'list (compose #'make-keyword #'string-upcase)
       (split-sequence separator string)))

(defun effective-options (options)
  "DOC"
  (remove-duplicates options
		     :key      #'car
		     :test     #'equal
		     :from-end t))
