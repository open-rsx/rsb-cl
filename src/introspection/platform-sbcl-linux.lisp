;;;; platform-sbcl-linux.lisp --- Platform-specific functions for SBCL on Linux.
;;;;
;;;; Copyright (C) 2014, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Process information

(defun current-process-id ()
  (sb-posix:getpid)) ; cannot fail according to getpid(2)

(defun current-program-name-and-commandline-arguments ()
  sb-ext:*posix-argv*)

(defun jiffies-per-second ()
  (with-platform-information-error-translation
      ("determine number of jiffies per second")
    (let ((result sb-alien::(alien-funcall (extern-alien "sysconf" (function long int)) 2)))
      (when (= -1 result)
        (error "~@<Syscall sysconf failed: ~A~@:>" (sb-unix::strerror)))
      result)))

(define-constant +boot-time-field-name+ "btime " :test #'string=)

(defun %boot-time/unix/seconds ()
  ;; The "btime" field in /proc/stat contains the system boot time in
  ;; seconds since UNIX epoch (See /proc/stat section of proc(5)).
  (with-platform-information-error-translation
      ("read btime from /proc/stat")
    (let* ((content (read-file-into-string "/proc/stat"))
           (line    (search +boot-time-field-name+ content))
           (start   (+ line (length +boot-time-field-name+)))
           (end     (position #\Newline content :start start)))
      (values (parse-integer (subseq content start end))))))

(define-constant +start-time-field-number+ 21 :test #'=)

(defun %start-time/boot/jiffies ()
  ;; Field 22 of /proc/PID/stat contains the start time of the process
  ;; in jiffies since boot (See /proc/[pid]/stat section in proc(5)).
  (with-platform-information-error-translation
      ("read starttime from /proc/self/stat")
    (with-input-from-file (stream "/proc/self/stat")
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (loop :repeat +start-time-field-number+ :do (read stream))
          (let ((value (read stream)))
            (check-type value (unsigned-byte 64))
            value))))))

(defun %current-process-start-time ()
  ;; Retrieve system boot time in seconds since UNIX epoch and process
  ;; start time in jiffies since system boot. Adding both to construct
  ;; a `local-time:timestamp' requires knowing the number of jiffies
  ;; per second. Since determining any of these can easily fail,
  ;; expect errors everywhere and return nil in case of errors.
  (with-platform-information-error-translation
      ("determine start time of the current process")
    (let+ ((start-time/unix/nanoseconds
            (* 1000000000
               (+ (%boot-time/unix/seconds)
                  (/ (%start-time/boot/jiffies) (jiffies-per-second)))))
           ((&values unix-seconds nanoseconds)
            (floor start-time/unix/nanoseconds 1000000000)))
      (local-time:unix-to-timestamp unix-seconds :nsec nanoseconds))))

;;; Host information

(defun %current-host-id ()
  (with-platform-information-error-translation
      ("determine unique id of the computer")
    (let+ (((&flet maybe-read-file (filename)
              (ignore-errors
                (when (probe-file filename)
                  (let* ((content (read-file-into-string filename))
                         (trimmed (remove #\Newline content)))
                    (unless (emptyp trimmed)
                      trimmed))))))
           (files '("/etc/machine-id"
                    "/var/lib/dbus/machine-id")))
      (or (some #'maybe-read-file files)
          (error "~@<Could not read machine id from any of the files ~
                  ~{~S~^, ~}~@:>"
                 files)))))
