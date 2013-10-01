;;;; configuration.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Environment variables

(defun options-from-environment ()
  "Obtain configuration options from environment variables."
  #+(and sbcl (not win32))
  (let+ (((&flet name->option-name (name)
            (when (starts-with-subseq "RSB_" name)
              (string->option-name (subseq name 4) #\_))))
         ((&flet variable->option (string)
            (let+ (((name value) (split-sequence #\= string :count 2))
                   (name (name->option-name name)))
              (when name
                (cons name value))))))
    (remove-if #'null (map 'list #'variable->option
                           (sb-impl::posix-environ))))
  #-(and sbcl (not win32))
  nil)

;;; Configuration file

(defun options-from-stream (stream)
  "Obtain configuration options from STREAM."
  (effective-options
   (iter (for  line     in-stream stream :using #'read-line)
         (for  line-num :from     1)
         (with section  =         nil)
         (let+ ((content (subseq line 0 (position #\# line)))
                ((&flet trim (string)
                   (string-trim '(#\Space) string))))
           (cond
             ;; Empty/comment-only line
             ((emptyp content))

             ;; Section header
             ((and (starts-with #\[ content) (ends-with #\] content))
              (setf section (string->option-name
                             (subseq content 1 (1- (length content))))))
             ;; Value
             ((= (funcall #'count #\= content) 1) ;; iterate :(
              (let+ (((name value)
                      (map 'list #'trim (split-sequence #\= content)))
                     (name (string->option-name name)))
                (collect (cons (append section name) value))))
             ;; Invalid
             (t
              (error "~@<Syntax error in line ~D, contents ~S.~@:>"
                     line-num line)))))))

;;;

(defun options-from-default-sources (&key
                                     (config-files *default-configuration-files*))
  "Combine options from the configuration sources mentioned in
CONFIG-FILES. Default:
+ System-wide rsb.conf file (e.g. /etc/rsb.conf on UNIX)
+ User-wide rsb.conf file (e.g. ~/.config/rsb.conf on UNIX)
+ $(PWD)/rsb.conf
+ Environment Variables"
  (apply #'merge-options
         (options-from-environment)
         (iter (for file in config-files)
               (with-open-file (stream file :if-does-not-exist nil)
                 (when stream
                   (collect (options-from-stream stream)))))))

;;;

(defun section-options (section &optional
                        (config *default-configuration*))
  (let+ ((section (ensure-list section))
         ((&flet strip-key (option)
            (cons (nthcdr (length section) (car option))
                  (cdr option)))))
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

(defun transport-options (&key
                          (config            *default-configuration*)
                          (exclude-disabled? t))
  "Collect and interpret options in CONFIG that apply to
transports. Options for transports which are disabled in CONFIG are
not returned."
  (let+ (((&flet options->plist (options)
            (iter (for (key . value) in options)
                  (when (and (length= 1 key)
                             (not (eq (first key) :enabled)))
                    (collect (first key)) (collect value)))))
         (options    (section-options :transport config))
         (transports (remove-duplicates
                      (map 'list (compose #'first #'car) options))))
    ;; Collect options for all individual transport. Skip disabled
    ;; transports.
    (iter (for transport in transports)
          (let ((options (section-options transport options)))
            (when (or (not exclude-disabled?)
                      (eq (option-value '(:enabled) nil options) t)
                      (string= (option-value '(:enabled) "0" options) "1")) ; TODO(jmoringe):
              (collect (cons transport (options->plist options))))))))

(defun process-transport-options (options)
  "If OPTIONS is of the form

  \(TRANSPORT KEY1 VALUE1 KEY2 VALUE2 ... &inherit)

replace &inherit with the default configuration options for
TRANSPORT. Otherwise return OPTIONS unmodified."
  (let+ (((transport &rest transport-options) options))
    (cons transport
          (if (ends-with '&inherit transport-options)
              (append (butlast transport-options)
                      (rest (find transport (transport-options)
                                  :key #'first)))
              transport-options))))

(defun default-converters (&key
                           (config *default-configuration*))
  "Return an alist of default converters for particular wire-types
with items of the form (WIRE-TYPE . CONVERTER).

If supplied, CONFIG specifies the configuration that should be used to
determine the set of default converters. if CONFIG is not supplied,
the value of `*default-configuration*' is used."
  '((nibbles:octet-vector . (:fundamental-void
                             :fundamental-bool
                             :fundamental-int64
                             :fundamental-uint32
                             :fundamental-int32
                             :fundamental-uint64
                             :fundamental-double
                             :fundamental-float
                             :fundamental-utf-8-string
                             :fundamental-bytes
                             :protocol-buffer))))

(defun default-converter (wire-type
                          &key
                          (config *default-configuration*))
  "Return the default converter for WIRE-TYPE.

If supplied, CONFIG specifies the configuration that should be used to
determine the set of default converters. if CONFIG is not supplied,
the value of `*default-configuration*' is used."
  (cdr (assoc wire-type (default-converters :config config))))

;;; Utility functions

(defun string->option-name (string &optional (separator #\.))
  "Split STRING at all occurrences of SEPARATOR, producing an option
name."
  (map 'list (compose #'make-keyword #'string-upcase)
       (split-sequence separator string)))

(defun merge-options (&rest options)
  "Return options that result from merging OPTIONS with options
occurring earlier taking precedence over later ones."
  (effective-options (apply #'append options)))

(defun effective-options (options)
  "Remove \"shadowed\" options in OPTIONS and return the resulting
options."
  (remove-duplicates options
                     :key      #'car
                     :test     #'equal
                     :from-end t))

;;; Set default configuration

(eval-when (:load-toplevel :execute)
  (setf *default-configuration* (options-from-default-sources)))
