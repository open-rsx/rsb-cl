;;;; git.lisp --- Utility functions for dealing with git.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb-config)

(defun git-describe (directory &key match)
  (inferior-shell:run/nil
   `("git" "describe" "--long" "--dirty=-dirty" "--tags"
           ,@(when match (list "--match" match)))
   :directory directory
   :output    '(:string :stripped t)))

(defun project-version-details (directory major minor)
  (handler-case
      (let* ((previous-release (format nil "release-~D.~D" major (1- minor)))
             (version-string   (git-describe directory :match previous-release)))
        (ppcre:register-groups-bind ((#'parse-integer revision) commit dirty)
            ("^release-[0-9]+\\.[0-9]+-([0-9]+)-(g[a-z0-9]+)(?:-(dirty))?" version-string)
          (values revision commit dirty)))
    (error ()
      (values 0 nil nil))))

(defun write-version-files (major minor revision commit dirty
                            details-file string-file)
  (flet ((write-file (file thunk)
           (uiop:with-safe-io-syntax ()
             (uiop:with-output-file (stream file
                                            :if-does-not-exist :create
                                            :if-exists         :supersede)
               (funcall thunk stream)))))
    (write-file details-file (lambda (stream)
                               (format stream "~D~%~D~%~D~%~A~@[~%dirty~]~%"
                                       major minor revision commit dirty)))
    (write-file string-file (lambda (stream)
                              (format stream "\"~D.~D.~D\"~%"
                                      major minor (or revision 0))))))

(defun update-version-file (input-file details-file string-file)
  (let ((directory (make-pathname :name nil :type nil :defaults input-file)))
    (destructuring-bind (major minor &optional revision commit dirty)
        (uiop:with-safe-io-syntax ()
          (uiop:read-file-forms input-file :count 5))
      (check-type major (integer 0))
      (check-type minor (integer 0))
      (cond
        (revision)
        ((uiop:directory-exists-p (merge-pathnames ".git/" directory))
         (setf (values revision commit dirty)
               (project-version-details directory major minor)))
        (t
         (error "~@<Could not determine project version.~@:>"))) ; TDOO
      (write-version-files major minor revision commit dirty
                           details-file string-file))))
