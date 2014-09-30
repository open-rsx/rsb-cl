;;;; platform-sbcl-win32.lisp --- Platform-specific functions for SBCL on Win32.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Process information

(defun current-process-id ()
  (sb-posix:getpid)) ; cannot fail

(defun current-program-name-and-commandline-arguments ()
  sb-ext:*posix-argv*)

;; Process start time

;; kernel32.dll; Cannot fail according to MSDN.
(sb-alien:define-alien-routine ("GetCurrentProcess" get-current-process)
  (* t))

(sb-alien:define-alien-type nil
  (sb-alien:struct filetime
    (low-date-time  sb-alien:long)
    (high-date-time sb-alien:long)))

(declaim (ftype (function ((sb-alien:alien (sb-alien:struct filetime))) integer)
                filetime/struct->filetime/integer))
(defun filetime/struct->filetime/integer (filetime)
  (let ((high (sb-alien:slot filetime 'high-date-time))
        (low  (sb-alien:slot filetime 'low-date-time)))
    (dpb high (byte 32 32) low)))

(define-constant +filetime-base-time+
    (local-time:parse-rfc3339-timestring "1601-01-01T00:00:00Z")
  :test #'local-time:timestamp=)

(defun filetime/integer->local-time (nanoseconds/100)
  (local-time:adjust-timestamp +filetime-base-time+
    (:offset :nsec (* 100 nanoseconds/100))))

;; kernel32.dll
(sb-alien:define-alien-routine ("GetProcessTimes" get-process-time)
    sb-alien:long
  (process (* t))
  (create  (* (sb-alien:struct filetime)))
  (exit    (* (sb-alien:struct filetime)))
  (kernel  (* (sb-alien:struct filetime)))
  (user    (* (sb-alien:struct filetime))))

(defun %current-process-start-time ()
  (with-platform-information-error-translation
      ("determine start time of the current process")
    (sb-alien:with-alien ((create (sb-alien:struct filetime))
                          (exit   (sb-alien:struct filetime))
                          (kernel (sb-alien:struct filetime))
                          (user   (sb-alien:struct filetime)))
      (if (zerop (get-process-time (get-current-process)
                                   (sb-alien:addr create) (sb-alien:addr exit)
                                   (sb-alien:addr kernel) (sb-alien:addr user)))
          (sb-win32::win32-error "GetProcessTimes")
          (filetime/integer->local-time
           (filetime/struct->filetime/integer create))))))

;;; Host information

(sb-alien:load-shared-object "secur32")

(sb-alien:define-alien-routine ("GetComputerObjectNameW" get-computer-object-name/w)
    sb-alien:int
  (format sb-alien:int)
  (buffer (sb-alien:array sb-alien:char 1024))
  (length (* sb-alien:long)))

(defun %current-host-id ()
  (with-platform-information-error-translation
      ("determine unique id of the computer")
    (sb-alien:with-alien ((buffer (sb-alien:array sb-alien:char 1024))
                          (length sb-alien:long                       :local 1024))
      (if (zerop (get-computer-object-name/w 6 buffer (sb-alien:addr length)))
          (sb-win32::win32-error "GetComputerObjectNameW")
          (values (sb-alien::c-string-to-string (alien-sap buffer) :utf-16le 'character) length)))))
