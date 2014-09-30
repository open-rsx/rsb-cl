;;;; platform-sbcl-darwin.lisp --- Platform-specific functions for SBCL on Darwin.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Process information

(defun current-process-id ()
  (sb-posix:getpid)) ; cannot fail according to getpid(2)

(defun current-program-name-and-commandline-arguments ()
  sb-ext:*posix-argv*)

(defun %current-process-start-time ()
  (with-platform-information-error-translation
      ("determine start time of the current process")
    (error "~@<Not available.~@:>")))

;;; Host information

(defun %current-host-id ()
  (with-platform-information-error-translation
      ("determine unique id of the computer")
    (sb-alien:with-alien ((timeout (sb-alien:struct sb-unix::timespec))
                          (uuid    (sb-alien:array sb-alien:unsigned-char 16)))
      (setf (sb-alien:slot timeout 'sb-unix::tv-sec)  1
            (sb-alien:slot timeout 'sb-unix::tv-nsec) 0)
      (sb-unix::syscall* ("gethostuuid" (* (array sb-alien:unsigned-char 16))
                                        (* (sb-alien:struct sb-unix::timespec)))
        (with-output-to-string (stream)
          (dotimes (i 16)
            (format stream "~(~2,'0X~)" (sb-alien:deref uuid i))))
        (sb-alien:addr uuid) (sb-alien:addr timeout)))))
