;;;; configuration.lisp --- Configuration of RSB participants.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Environment variables

(defun options-from-environment ()
  "Obtain configuration options from environment variables."
  #+sbcl
  (let+ (((&flet name->option-name (name)
            (when (starts-with-subseq "RSB_" name)
              (string->option-name (subseq name 4) #\_))))
         ((&flet variable->option (string)
            (let+ (((name value) (split-sequence #\= string :count 2))
                   (name (name->option-name name)))
              (when name
                (cons name value))))))
    (remove-if #'null (mapcar #'variable->option (sb-ext:posix-environ)))))

;;; Configuration file

(defun options-from-stream (stream)
  "Obtain configuration options from STREAM."
  (effective-options
   (iter (for  line     in-stream stream :using #'read-line)
         (for  line-num :from     1)
         (with section  =         nil)
         (let+ (((&flet trim (string)
                   (string-trim '(#\Space #\Tab) string)))
                (content (trim (subseq line 0 (position #\# line)))))
           (cond
             ;; Empty/comment-only line
             ((emptyp content))

             ;; Section header
             ((and (starts-with #\[ content) (ends-with #\] content))
              (setf section (string->option-name
                             (subseq content 1 (1- (length content))))))
             ;; Value
             ((find #\= content)
              (let+ ((index (position #\= content))
                     (name (string->option-name
                            (trim (subseq content 0 index))))
                     (value (trim (subseq content (1+ index)))))
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
  (reduce #'merge-options
          (list* (options-from-environment)
                 (iter (for file in config-files)
                       (with-open-file (stream file :if-does-not-exist nil)
                         (when stream
                           (collect (options-from-stream stream))))))
          :initial-value *default-configuration*
          :from-end      t))

;;;

(defun section-options (section &optional (config *configuration*))
  (let+ ((section (ensure-list section))
         ((&flet strip-key (option)
            (cons (nthcdr (length section) (car option))
                  (cdr option)))))
    (mapcar #'strip-key
            (remove section config
                    :test-not #'starts-with-subseq
                    :key      #'car))))

(defun option-value (name &optional default (config *configuration*))
  "Return the value of the option named NAME in CONFIG.
   Return DEFAULT, if NAME does not exist in CONFIG."
  (if-let ((option (assoc name config :test #'equal)))
    (cdr option)
    default))

;;; Transport configuration

(defun transport-options (&optional (config *configuration*))
  "Collect and return options in CONFIG that apply to transports."
  (let+ (((&flet options->plist (options)
            (iter (for (key . value) in options)
                  (cond
                    ((length= 1 key)
                     (collect (first key))
                     (collect value))
                    ((starts-with-subseq '(:converter :lisp) key)
                     ;; Ignore converter configuration for now.
                     )))))
         (options    (section-options :transport config))
         (transports (remove-duplicates
                      (mapcar (compose #'first #'car) options))))
    ;; Collect options for all individual transport.
    (mapcar (lambda (transport)
              (cons transport (options->plist
                               (section-options transport options))))
            transports)))

(let+ (((&flet find-transport (name options)
          (find name options :test #'eq :key #'first)))
       ((&flet inherit? (options)
          (ends-with '&inherit options)))
       ((&flet enabled? (options)
          (member (getf options :enabled) '(t "1") :test #'equal)))
       ((&flet deinherit (options)
          (if (inherit? options)
              (butlast options)
              options)))
       ((&flet inherit (options defaults)
          (let ((result (copy-list (deinherit defaults))))
            (iter (for (value key) on (reverse (deinherit options)) :by #'cddr)
                  (setf (getf result key) value))
            result)))
       ((&flet+ merge-entries ((transport1 &rest options)
                               (transport2 &rest defaults))
          (list* (if (eq transport1 t) transport2 transport1)
                 (if (inherit? options)
                     (append (inherit options defaults)
                             (when (inherit? defaults)
                               '(&inherit)))
                     options)))))

  ;; Assumes that &inherit has already been processed (but &inherit
  ;; markers may still be present).
  (defun effective-transport-options (options)
    (iter (for (transport . options*) in options)
          (let ((options* (deinherit options*)))
            (unless (or (eq transport t) (not (enabled? options*)))
              (let ((effective-options (remove-from-plist options* :enabled)))
                (collect (list* transport effective-options)))))))

  (defun merge-transport-options (options defaults)
    (let+ ((generic (find-transport t options))
           (rest    (if generic
                        (mapcar (rcurry #'merge-entries generic) options)
                        (copy-list options)))
           ((&flet merge-one-default (default)
              (let ((intermediate (if generic
                                      (merge-entries generic default)
                                      default)))
                (if-let ((entry (find-transport (first intermediate) rest)))
                  (progn
                    (removef rest entry)
                    (merge-entries entry intermediate))
                  intermediate)))))
      (append (mapcar #'merge-one-default defaults) rest))))

;;; Converter configuration

(defvar *default-converters*
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
                             :fundamental-scope
                             :protocol-buffer))))

(defun default-converters (&key (config *configuration*))
  "Return an alist of default converters for particular wire-types
   with items of the form (WIRE-TYPE . CONVERTER).

   If supplied, CONFIG specifies the configuration that should be used
   to determine the set of default converters. if CONFIG is not
   supplied, the value of `*configuration*' is used."
  *default-converters*)

(defun default-converter (wire-type &key (config *configuration*))
  "Return the default converter for WIRE-TYPE.

   If supplied, CONFIG specifies the configuration that should be used
   to determine the set of default converters. if CONFIG is not
   supplied, the value of `*configuration*' is used."
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
  (setf *configuration* (options-from-default-sources)))
