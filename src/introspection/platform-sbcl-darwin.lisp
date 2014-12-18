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

;; This is not the actual definition of the kproc structure, but we
;; are only interested in the start time.
(sb-alien:define-alien-type nil
  (sb-alien:struct kproc
    (start-time (sb-alien:struct sb-unix:timeval))
    (padding    (array sb-alien:char 632))))

(defun %current-process-start-time ()
  (with-platform-information-error-translation
      ("determine start time of the current process")
    (sb-alien:with-alien ((mib      (array sb-alien:int 16))
                          (mib-size sb-alien:size-t         16))
      ;; Translate the string name into a sequence of numeric ids.
      (sb-unix::syscall* ("sysctlnametomib" sb-alien:c-string (* sb-alien:int) (* sb-alien:size-t))
          (values)
        "kern.proc.pid"
        (sb-alien:cast mib (* sb-alien:int)) (sb-alien:addr mib-size))
      ;; Add the process id as the fourth element of the mib. Increase
      ;; the size correspondingly.
      (setf (sb-alien:deref mib mib-size) (sb-posix:getpid))
      (incf mib-size)
      ;; Return information block and extract start time.
      (sb-alien:with-alien
          ((kproc      (sb-alien:struct kproc))
           (kproc-size sb-alien:size-t         (sb-alien:alien-size
                                                (sb-alien:struct kproc))))
        (sb-unix::syscall* ("sysctl" (* sb-alien:int) sb-alien:size-t
                                     (* t) (* sb-alien:size-t)
                                     (* t) sb-alien:size-t)
            (let* ((start-time    (sb-alien:slot kproc 'start-time))
                   (seconds       (sb-alien:slot start-time 'sb-unix::tv-sec))
                   (micro-seconds (sb-alien:slot start-time 'sb-unix::tv-usec)))
              (local-time:unix-to-timestamp seconds :nsec (* 1000 micro-seconds)))
          (sb-alien:cast mib (* sb-alien:int)) mib-size
          (sb-alien:addr kproc) (sb-alien:addr kproc-size)
          (sb-sys:int-sap 0) 0)))))

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
