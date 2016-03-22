;;;; package.lisp --- Package definition for unit tests of the converter module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:defpackage #:rsb.converter.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions
   #:lift

   #:nibbles

   #:rsb
   #:rsb.converter

   #:rsb.test)

  (:export
   #:converter-root
   #:octetify

   #:define-basic-converter-test-cases)

  ;; Test utilities
  (:export
   #:call-tracking-converter
   #:converter-next
   #:converter-calls)

  (:documentation
   "This package contains unit tests for the converter module."))

(cl:in-package #:rsb.converter.test)

(deftestsuite converter-root (root)
  ()
  (:function
   (octetify (value)
     (coerce value 'simple-octet-vector)))
  (:documentation
   "Root unit test suite for the converter module."))

(defmacro define-basic-converter-test-cases
    ((converter
      &key
      (suite-name     (symbolicate converter "-ROOT"))
      (make-converter converter)
      (domain-test    'equalp))
      cases)
  "Emit basic test cases for CONVERTER in test suite SUITE-NAME."
  `(progn

     (addtest (,suite-name
               :documentation
               ,(format nil "Test methods on `wire->domain?' for the `~(~A~)' converter."
                        converter))
       wire->domain-applicability

       (ensure-cases (wire-data wire-schema domain-object)
           ,cases
         (let+ ((converter ,make-converter)
                ((&flet do-it ()
                   (wire->domain? converter wire-data wire-schema))))
           (cond
             ((member wire-data '(:error :not-applicable)))
             ((eq domain-object :not-applicable)
              (ensure-null (do-it)))
             (t
              (ensure-same (do-it) converter :ignore-multiple-values? t))))))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test methods on `domain->wire?' for the `~(~A~)' converter."
                        converter))
       domain->wire-applicability

       (ensure-cases (wire-data wire-schema domain-object)
           ,cases
         (let+ ((converter ,make-converter)
                ((&flet do-it ()
                   (domain->wire? converter domain-object))))
          (cond
            ((member domain-object '(:error :not-applicable)))
            ((eq wire-data :not-applicable)
             (ensure-null (do-it)))
            (t
             (let+ (((&values converter* wire-type* wire-schema*) (do-it)))
               (ensure-same converter converter*)
               (unless (eq wire-data :error)
                 (ensure (typep wire-data wire-type*)))
               (ensure-same wire-schema wire-schema*)))))))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test methods on `wire->domain' for the `~(~A~)' converter."
                        converter))
       wire->domain-smoke

       (ensure-cases (wire-data wire-schema domain-object)
           ,cases
         (let+ ((converter ,make-converter)
                ((&flet do-it ()
                   (wire->domain converter wire-data wire-schema))))
          (cond
            ((member wire-data '(:error :not-applicable)))
            ((member domain-object '(:error :not-applicable))
             (ensure-condition 'error (do-it)))
            (t
             (ensure-same (do-it) domain-object
                          :test (function ,domain-test)))))))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test methods on `domain->wire' for the `~(~A~)' converter."
                        converter))
       domain->wire-smoke

       (ensure-cases (wire-data wire-schema domain-object)
           ,cases
         (let+ ((converter ,make-converter)
                ((&flet do-it ()
                   (domain->wire converter domain-object))))
          (cond
            ((member domain-object '(:error :not-applicable)))
            ((member wire-data '(:error :not-applicable))
             (ensure-condition 'error (do-it)))
            (t
             (ensure-same (do-it) (values wire-data wire-schema)
                          :test #'equalp))))))

     (addtest (,suite-name
               :documentation
               ,(format nil "Roundtrip test for the `~(~A~)' converter."
                        converter))
       roundtrip

       (ensure-cases (wire-data wire-schema domain-object)
           ,cases
         (unless (or (member wire-data '(:error :not-applicable))
                     (member domain-object '(:error :not-applicable)))
           (let+ ((converter ,make-converter)
                  ((&values encoded encoded-wire-schema)
                   (domain->wire converter domain-object))
                  (decoded (wire->domain converter encoded encoded-wire-schema)))
             (ensure-same domain-object decoded
                          :test (function ,domain-test))))))))

;;; `tracking-converter'

(defclass call-tracking-converter ()
  ((next  :initarg  :next
          :reader   converter-next
          :documentation
          "The subordinate converter to delegate the actual work to.")
   (calls :type     list
          :reader   converter-calls
          :accessor converter-%calls
          :initform '()
          :documentation
          "A list of calls to converter-protocol-related functions."))
  (:default-initargs
   :next (missing-required-initarg 'call-tracking-converter :next))
  (:documentation
   "A converter that tracks convert-protocol-related calls but
    otherwise delegates to a subordinate converter."))

(macrolet ((define-call-tracking-method (name lambda-list)
             (let* ((unspecialized
                     (mapcar (compose #'first #'ensure-list)
                             (parse-ordinary-lambda-list
                              lambda-list :allow-specializers t)))
                    (converter (first unspecialized)))
               `(defmethod ,name ,lambda-list
                  (push `(,',name ,,@unspecialized)
                        (converter-%calls ,converter))
                  (,name (converter-next ,converter)
                         ,@(rest unspecialized))))))

  (define-call-tracking-method wire->domain?
      ((converter   call-tracking-converter)
       (wire-data   t)
       (wire-schema t)))

  (define-call-tracking-method domain->wire?
      ((converter     call-tracking-converter)
       (domain-object t)))

  (define-call-tracking-method wire->domain
      ((converter   call-tracking-converter)
       (wire-data   t)
       (wire-schema t)))

  (define-call-tracking-method domain->wire
      ((converter     call-tracking-converter)
       (domain-object t))))

(service-provider:register-provider/class
 'rsb.converter::converter :call-tracking :class 'call-tracking-converter)
