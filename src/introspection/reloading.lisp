;;;; reloading.lisp --- Support for releading images with RSB introspection.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

(defun reinitialize-introspection ()
  ;; Note that the body is only executed when there already is a local
  ;; database object. Otherwise, nothing happens.
  (with-local-database (database)
    (let+ (((&structure introspection- process host) database))
      (setf host    (current-host-info)
            process (current-process-info))
      (log:info "~@<Reinitialized ~A with ~A and ~A~@:>"
                database host process))))

#+sbcl (pushnew 'reinitialize-introspection sb-ext:*init-hooks*)
#-sbcl (pushnew 'reinitialize-introspection uiop:*image-restore-hook*)
