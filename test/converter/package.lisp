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

   #:fiveam

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

(def-suite converter-root
  :in root
  :description
  "Root unit test suite for the converter module.")

(defun octetify (value)
  (coerce value 'simple-octet-vector))

(defmacro define-basic-converter-test-cases
    ((converter
      &key
      suite
      (make-converter converter)
      (domain-test    'equalp)
      (simple?        t))
     cases)
  "Emit basic test cases for CONVERTER in test suite SUITE-NAME."
  `(progn

     (test (wire->domain-applicability
            ,@(when suite `(:suite ,suite)))
       ,(format nil "Test methods on `wire->domain?' for the `~(~A~)' converter."
                converter)

       (mapc
        (lambda+ ((wire-data wire-schema domain-object))
          (let+ ((converter ,make-converter)
                 ((&flet do-it ()
                    (wire->domain? converter wire-data wire-schema))))
            (cond
              ((member wire-data '(:error :not-applicable)))
              ((eq domain-object :not-applicable)
               (is (null  (do-it))))
              (t
               ,(if simple?
                    `(is (eql converter (do-it)))
                    `(is-true (do-it)))))))
        ,cases))

     (test (domain->wire-applicability
            ,@(when suite `(:suite ,suite)))
       ,(format nil "Test methods on `domain->wire?' for the `~(~A~)' converter."
                converter)

       (mapc
        (lambda+ ((wire-data wire-schema domain-object))
          (let+ ((converter ,make-converter)
                 ((&flet do-it ()
                    (domain->wire? converter domain-object))))
            (cond
              ((member domain-object '(:error :not-applicable)))
              ((eq wire-data :not-applicable)
               (is (null (do-it))))
              (t
               (let+ (((&values converter* wire-type* wire-schema*) (do-it)))
                 ,(if simple?
                      `(is (eql converter converter*))
                      `(is-true converter*))
                 (unless (eq wire-data :error)
                   (is (typep wire-data wire-type*)))
                 (is (eql wire-schema wire-schema*)))))))
        ,cases))

     (test (wire->domain-smoke
            ,@(when suite `(:suite ,suite)))
       ,(format nil "Test methods on `wire->domain' for the `~(~A~)' converter."
                converter)

       (mapc (lambda+ ((wire-data wire-schema domain-object))
               (let+ ((converter ,make-converter)
                      ((&flet do-it ()
                         (wire->domain converter wire-data wire-schema))))
                 (cond
                   ((member wire-data '(:error :not-applicable)))
                   ((member domain-object '(:error :not-applicable))
                    (signals error (do-it)))
                   (t
                    (is (,domain-test domain-object (do-it)))))))
             ,cases))

     (test (domain->wire-smoke
            ,@(when suite `(:suite ,suite)))
       ,(format nil "Test methods on `domain->wire' for the `~(~A~)' converter."
                converter)

       (mapc (lambda+ ((wire-data wire-schema domain-object))
               (let+ ((converter ,make-converter)
                      ((&flet do-it ()
                         (domain->wire converter domain-object))))
                 (cond
                   ((member domain-object '(:error :not-applicable)))
                   ((member wire-data '(:error :not-applicable))
                    (signals error (do-it)))
                   (t
                    (is (equalp (list wire-data wire-schema)
                                (multiple-value-list (do-it))))))))
             ,cases))

     (test (roundtrip
            ,@(when suite `(:suite ,suite)))
       ,(format nil "Roundtrip test for the `~(~A~)' converter."
                converter)

       (mapc
        (lambda+ ((wire-data wire-schema domain-object))
          (unless (or (member wire-data '(:error :not-applicable))
                      (member domain-object '(:error :not-applicable)))
            (let+ ((converter ,make-converter)
                   ((&values encoded encoded-wire-schema)
                    (domain->wire converter domain-object))
                   (decoded (wire->domain converter encoded encoded-wire-schema)))
              (is (,domain-test domain-object decoded))
              (is (eql wire-schema encoded-wire-schema)))))
        ,cases))))

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
