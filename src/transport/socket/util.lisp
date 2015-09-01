;;;; util.lisp --- Utilities used in the transport.socket module
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

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

(defun maybe-write-port-file (filename bus &optional existing)
  (when (and filename (not (equalp filename existing)))
    (let+ ((port (usocket:get-local-port (bus-socket bus)))
           ((&flet print-it (stream)
              (with-standard-io-syntax
                (prin1 port stream)
                (terpri stream)
                (force-output stream)))))
      (log:debug "~@<Writing port ~D to ~S.~@:>" port filename)
      (switch (filename :test #'equal)
        ("-"
         (print-it *standard-output*))
        ("-2"
         (print-it *error-output*))
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
