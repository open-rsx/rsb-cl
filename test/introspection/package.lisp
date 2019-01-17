;;;; package.lisp --- Package definition for unit tests of the introspection module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:fiveam

   #:rsb
   #:rsb.patterns.request-reply
   #:rsb.introspection

   #:rsb.test)

  (:shadow
   #:run-tests)

  ;; Platform information functions
  (:import-from #:rsb.introspection
   #:current-process-id
   #:current-program-name-and-commandline-arguments
   #:current-process-start-time

   #:current-host-id

   #:remote-introspection-database

   #:*local-database*)

  ;; Root test suite
  (:export
   #:introspection-root

   #:run-tests)

  (:documentation
   "This package contains unit tests for the introspection module."))

(cl:in-package #:rsb.introspection.test)

;;; Utilities

(defparameter +introspection-configuration+
  '(((:introspection :enabled)        . t)
    ((:transport :socket :enabled)    . nil)
    ((:transport :inprocess :enabled) . t)))

(defun make-introspection-scope (&optional (suffix '()))
  (merge-scopes suffix +introspection-scope+))

(defun make-introspection-participants-scope (&optional (suffix '()))
  (merge-scopes suffix +introspection-participants-scope+))

(defun send-introspection-event (datum &rest args &key
                                 suffix-scope
                                 &allow-other-keys)
  (let ((scope (make-introspection-participants-scope suffix-scope)))
    (with-participant (informer :informer scope :introspection? nil)
      (apply #'send informer datum (remove-from-plist args :suffix-scope)))))

(defmacro with-condition-tracking ((handler-name check-conditions-name
                                    &optional
                                    (clear-calls-name (gensym)))
                                   &body body)
  (with-unique-names (calls)
    `(let+ ((,calls '())
            ((&labels ,handler-name (condition)
               (appendf ,calls (list condition))
               (continue condition)))
            ((&labels ,check-conditions-name (expected)
               (let ((num-calls    (length ,calls))
                     (num-expected (length expected)))
                 (is (= num-expected num-calls)
                     "~@<~D call~:P but expected ~D~:@>"
                     num-calls num-expected))
               (mapc (lambda+ (call (expected-type &rest expected-substrings))
                       (is (typep call expected-type))
                       (let* ((*print-right-margin* most-positive-fixnum)
                              (*print-length*       most-positive-fixnum)
                              (text (princ-to-string call)))
                         (dolist (expected-substring expected-substrings)
                           (is (search expected-substring text)))))
                     ,calls expected)
              (,clear-calls-name)))
           ((&labels ,clear-calls-name ()
              (setf ,calls '()))))
       ,@body)))

;;; Test suite

(def-suite introspection-root
  :in root
  :description
  "Root unit test suite of the introspection module.")
#+TODO   (:dynamic-variables
          (rsb:*configuration* +introspection-configuration+))

(defun run-tests ()
  (run! 'introspection-root))
