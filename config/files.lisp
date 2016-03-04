;;;; files.lisp --- Utilities for handling files.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb-config)

(defun describe-copy-file (from to &key (stream *trace-output*))
  (flet ((dir (components &rest args)
           (let ((directory (if (member (first components)
                                        '(:absolute :relative))
                                components
                                (list* :relative components))))
             (apply #'make-pathname :directory directory args))))
    (let* ((from-dir (pathname-directory from))
           (to-dir   (pathname-directory to))
           (start    (mismatch from-dir to-dir :test #'equal))
           (end-a    (mismatch from-dir to-dir :from-end t :test #'equal))
           (end-b    (mismatch to-dir from-dir :from-end t :test #'equal))
           (prefix   (dir (subseq from-dir 0 start)))
           (var-a    (dir (subseq from-dir start end-a)))
           (var-b    (dir (subseq to-dir   start end-b)))
           (suffix   (dir (subseq from-dir end-a) :defaults from)))
      (format stream "~&; Copying ~A{~A -> ~A}~A~%" prefix var-a var-b suffix))))

(defun copy-file (from to)
  (describe-copy-file from to)
  (ensure-directories-exist to)
  (uiop:copy-file from to))
