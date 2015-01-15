;;;; logger.lisp --- An example program demonstrating introspection.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:cl-user)

;;;; Loading this file does not terminate.

;; mark-start::body
#.(progn (require :rsb-introspection) (values))

(labels ((log-event (kind object event subject)
           (format t "~8A ~56A ~20A ~A~%" kind object event subject))
         (on-process-change (object subject event)
           (log-event :process object event subject))
         (on-host-change (object subject event)
           (log-event :host object event subject)
           (flet ((hook () (rsb.introspection:database-change-hook subject)))
             (case event
               (:process-added
                (hooks:add-to-hook
                 (hook) (alexandria:curry #'on-process-change subject)))
               (:process-removed
                (hooks:clear-hook (hook))))))
         (on-database-change (object subject event)
           (log-event :database object event subject)
           (flet ((hook () (rsb.introspection:database-change-hook subject)))
             (case event
               (:host-added
                (hooks:add-to-hook
                 (hook) (alexandria:curry #'on-host-change subject)))
               (:host-removed
                (hooks:clear-hook (hook)))))))
  (rsb:with-participant (introspection
                         :remote-introspection rsb.introspection:+introspection-scope+
                         :change-handler (alexandria:curry #'on-database-change :introspection))
    (declare (ignore introspection))
    (sleep most-positive-fixnum)))
;; mark-end::body
