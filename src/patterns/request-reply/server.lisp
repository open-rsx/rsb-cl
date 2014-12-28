;;;; server.lisp --- A superclass for local and remote server classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; `method1' class

(defclass method1 (participant
                   composite-participant-mixin
                   lazy-child-making-mixin
                   child-container-mixin
                   configuration-inheritance-mixin)
  ((name :initarg  :name
         :type     (or null method-name)
         :reader   method-name
         :documentation
         "Stores the name of the method."))
  (:default-initargs
   :name   (missing-required-initarg 'method1 :name)
   :server (missing-required-initarg 'method1 :server))
  (:documentation
   "This class serves as a superclass for local and remote method
    classes."))

(defmethod shared-initialize :before
    ((instance   method1)
     (slot-names t)
     &key
     (name             nil name-supplied?)
     server
     (transform-option nil transform-option-supplied?))
  (declare (ignore server))
  (when (and name-supplied? name)
    (check-type name method-name "a legal method name"))
  (when transform-option-supplied?
    (check-type transform-option transform-specification)))

(defmethod print-object ((object method1) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (method-name object))))

;;; `server' class

(defclass server (participant
                  composite-participant-mixin
                  child-container-mixin
                  configuration-inheritance-mixin)
  ((method-kind :type     keyword
                :allocation :class
                :reader   server-method-kind
                :initform :method)) ; useful for tests
  (:documentation
   "This class serves as a superclass for local and remote server
    classes. It provides storage of transport options and methods and
    generic support for retrieving, adding and removing methods."))

(defmethod shared-initialize :before
    ((instance   server)
     (slot-names t)
     &key
     (transform-option nil transform-option-supplied?))
  (when transform-option-supplied?
    (check-type transform-option transform-specification)))

(macrolet
    ((define-child-method (which)
       `(defmethod ,(ecase which
                      (:get  'participant-child)
                      (:set  '(setf participant-child))
                      (:make 'make-child-participant))
            :before (,@(case which
                         (:set '((new-value method1))))
                     (participant server)
                     (which       string)
                     (kind        t)
                     &key &allow-other-keys)
            (check-type which method-name "a legal method name"))))
  (define-child-method :get)
  (define-child-method :set)
  (define-child-method :make))

(defmethod make-child-initargs ((participant server)
                                (which       t)
                                (kind        t)
                                &key)
  (list* :name   which
         :server participant
         (call-next-method)))

(defmethod server-methods ((server server))
  (participant-children server))

(flet ((get-method (server name error?)
         (participant-child
          server name (server-method-kind server)
          :if-does-not-exist
          (when error?
            (lambda (condition)
              (error 'no-such-method
                     :container (child-condition-container condition)
                     :key       (child-condition-key       condition)))))))
  (macrolet
      ((define-server-method-method (name-specializer)
         `(defmethod server-method ((server server)
                                    (name   ,name-specializer)
                                    &key
                                    (error? t))
            (get-method server name error?))))

    (define-server-method-method string)
    (define-server-method-method (eql nil))))

(macrolet
    ((define-setf-server-method-method (name-specializer new-value-specializer)
       `(defmethod (setf server-method) ((new-value ,new-value-specializer)
                                         (server    server)
                                         (name      ,name-specializer)
                                         &key
                                         argument)
          (declare (ignore argument))
          (setf (participant-child server name (server-method-kind server))
                new-value))))

  (define-setf-server-method-method string    method1)
  (define-setf-server-method-method (eql nil) method1)

  (define-setf-server-method-method string    (eql nil))
  (define-setf-server-method-method (eql nil) (eql nil)))
