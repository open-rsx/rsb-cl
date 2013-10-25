;;;; conditions.lisp --- Conditions used in the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

;;; Data-flow conditions

(define-condition flow-condition (rsb-condition)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(define-condition suspended (flow-condition)
  ((participant :initarg  :participant
                :reader participant
                :documentation
                ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<An attempt was made to use the suspended ~
                     participant ~A.~@:>"
             (participant condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition send-while-suspended (suspended)
  ((event :initarg  :event
          :reader   event
          :documentation
          ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<An attempt was made to send the event ~A ~
                     through the suspended participant ~A.~@:>"
             (event       condition)
             (participant condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition remote-flow-condition (flow-condition)
  ((peer :initarg  :peer
         :type     uuid:uuid
         :reader   peer
         :documentation
         ""))
  (:default-initargs
   :peer (missing-required-initarg 'remote-flow-condition :peer))
  (:report
   (lambda (condition stream)
     (format stream "~@<A watermark has been reached in peer ~A.~@:>"
             (peer condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition watermark-condition (flow-condition)
  ((which :initarg  :which
          :type     keyword
          :reader   which
          :documentation
          ""))
  (:default-initargs
   :peer (missing-required-initarg 'watermark-condition :which))
  (:report
   (lambda (condition stream)
     (format stream "~@<A ~S watermark has been reached.~@:>"
             (which condition))))
  (:documentation
   "TODO(jmoringe): document"))

(macrolet
    ((define-watermark-conditions (name)
       (let ((name/basic  (format-symbol *package* "~A-WATERMARK-REACHED" name))
             (name/remote (format-symbol *package* "~A-WATERMARK-REACHED/REMOTE" name)))
         `(progn
            (define-condition ,name/basic (watermark-condition)
              ()
              (:report
               (lambda (condition stream)
                 (format stream ,(format nil "~~@<~@(~A~) ~~S watermark reached.~~@:>" name)
                         (which condition))))
              (:documentation
               "TODO(jmoringe): document"))

            (define-condition ,name/remote (,name/basic
                                            remote-flow-condition)
              ()
              (:report
               (lambda (condition stream)
                 (format stream ,(format nil "~~@<~@(~A~) ~~S watermark reached in peer ~~A.~~@:>"
                                         name)
                         (which condition)
                         (peer  condition))))
              (:documentation
               "TODO(jmoringe): document"))))))

  (define-watermark-conditions high)
  (define-watermark-conditions low))

;;; Data-flow participant condition

(define-condition source-creation-error (informer-creation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a source
    fails."))

(define-condition sink-creation-error (listener-creation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a sink fails."))
