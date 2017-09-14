;;;; util.lisp --- Utilities used in the transport.socket module
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; Bus locking

(defmacro with-locked-bus ((bus
                            &rest args
                            &key
                            connections?
                            connectors?)
                           &body body)
  "Execute BODY with BUS' lock held."
  (check-type connections? boolean)
  (check-type connectors?  boolean)

  (flet ((maybe-with-lock (which requested? body)
           (if requested?
               `((bt:with-recursive-lock-held ((,which ,bus))
                   ,@body))
               body)))
    `(progn
       ,@(maybe-with-lock
          'bus-connections-lock (or (not args) connections?)
          (maybe-with-lock 'bus-connectors-lock (or (not args) connectors?)
                           body)))))

;;; Connection options

(defun make-connection-options (connector)
  "Return a plist of connection options that should be used by
   connections associated to CONNECTOR."
  (let+ (((&structure-r/o connector- portfile nodelay?) connector))
    (list* :nodelay? nodelay?
           (when portfile
             (list :portfile portfile)))))

(defun check-connection-options (bus-options options)
  "Signal an error if the option plists BUS-OPTIONS and OPTIONS
   contain conflicting properties."
  (with-simple-restart (continue "~@<Ignore the incompatibility and ~
                                  use the existing bus object.~@:>")
    (iter (for (key value) on options :by #'cddr)
          (when (eq key :portfile)
            (next-iteration))
          (let ((bus-value (getf bus-options key)))
            (unless (equalp bus-value value)
              (error "~@<Incompatible values for option ~S: current ~
                      bus uses value ~S; requested value is ~S.~@:>"
                     key bus-value value))))))

(#-sbcl defvar #+sbcl sb-ext:defglobal **written-port-files**
  ;; Associates to `bus' instances lists of port-files that have been
  ;; written for the respective instances.
  (tg:make-weak-hash-table :test #'eq :weakness :key))

(defun should-write-port-file? (filename bus)
  ;; A port-file should be written if FILENAME is true and the same
  ;; port-file has not already been written for BUS.
  ;;
  ;; Additional locking is not necessary since this is always executed
  ;; under the transport lock.
  (when (and filename
             (not (member filename (gethash bus **written-port-files**)
                          :test #'equalp)))
    (push filename (gethash bus **written-port-files** '()))
    t))

(defun maybe-write-port-file (filename bus)
  (when (should-write-port-file? filename bus)
    (let+ ((port (usocket:get-local-port (bus-socket bus)))
           ((&flet print-it (stream)
              (with-standard-io-syntax
                (prin1 port stream)
                (terpri stream)
                (force-output stream)))))
      (log:debug "~@<Writing port ~D to ~S.~@:>" port filename)
      (cond
        ((equal filename "-")
         (print-it *standard-output*))
        ((equal filename "-2")
         (print-it *error-output*))
        #+(and sbcl (not win32))
        ((starts-with #\- filename)
         (let ((fd (parse-integer filename :start 1)))
           (log:debug "~@<Trying to write port ~D to file descriptor ~
                       ~:D.~@:>"
                      port fd)
           (with-open-stream (stream (sb-sys:make-fd-stream fd :output t))
             (print-it stream))))
        ((starts-with-subseq "call:" filename) ; for unit tests
         (funcall (read-from-string (subseq filename 5)) port))
        (t
         (with-output-to-file (stream filename :if-exists :supersede)
           (print-it stream)))))))

;;; Printing utility functions

(defun print-socket (stream socket &optional colon? at?)
  "Print ENDPOINT to STREAM."
  (declare (ignore at?))
  (format stream "~{~:[ADDRESS?~;~:*~A~]:~:[PORT?~;~:*~D~]~}"
          (handler-case (multiple-value-list
                         (if colon?
                             (usocket:get-local-name socket)
                             (usocket:get-peer-name  socket)))
            (error () '(nil nil)))))
