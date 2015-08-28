;;;; util.lisp --- Utilities used in the model.inference module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.inference)

;;; Minimal tri-modal logic
;;;
;;; See documentation string of `communication?' for an explanation of
;;; how the three states are used in this module.

(defun check-tri-value (value definitive?)
  (when (and value (not definitive?))
    (error 'incompatible-arguments
           :parameters '(:value :definitive?)
           :values     (list value definitive?))))

(declaim (inline funcall-and-check))
(defun funcall-and-check (thunk)
  (let+ (((&values value definitive?) (funcall thunk)))
    (check-tri-value value definitive?)
    (values value definitive?)))

(declaim (ftype (function (function function) (values boolean boolean &optional))
                %tri-and %tri-or))

(defun %tri-and (left-thunk right-thunk)
  (let+ (((&values left-result left-definitive?)
          (funcall-and-check left-thunk)))
    (if (and (not left-result) left-definitive?)
        (values nil t)
        (let+ (((&values right-result right-definitive?)
                (funcall-and-check right-thunk)))
          (if (and (not right-result) right-definitive?)
              (values nil t)
              (values (and left-result right-result)
                      (and left-definitive? right-definitive?)))))))

(defun %tri-or (left-thunk right-thunk)
  (let+ (((&values left-result left-definitive?)
          (funcall-and-check left-thunk)))
    (if (and left-result left-definitive?)
        (values t t)
        (let+ (((&values right-result right-definitive?)
                (funcall-and-check right-thunk)))
          (if (and right-result right-definitive?)
              (values t t)
              (values (or left-result right-result)
                      (and left-definitive? right-definitive?)))))))

(macrolet
   ((define-operator-macro (macro-name function-name)
      `(defmacro ,macro-name (&optional
                              (left  nil left-supplied?)
                              (right nil right-supplied?)
                              &rest more)
         (cond
           (more
            `(,',macro-name
              (,',function-name (lambda () ,left) (lambda () ,right))
              ,@more))
           (right-supplied?
            `(,',function-name (lambda () ,left) (lambda () ,right)))
           (left-supplied?
            left)
           (t
            `(values t t))))))
  (define-operator-macro tri-and %tri-and)
  (define-operator-macro tri-or  %tri-or))

(defun tri-reduce (function sequence
                   &key
                   (key           #'values-list)
                   (initial-value '(nil t)))
  (let ((function (coerce function 'function))
        (key      (coerce key 'function)))
    (values-list
     (reduce (lambda (element accum)
               (multiple-value-list
                (multiple-value-call function
                  (lambda () (funcall key element))
                  (lambda () (values-list accum)))))
             sequence :initial-value initial-value :from-end t))))

(declaim (inline tri-some))
(defun tri-some (function sequence)
  (tri-reduce #'%tri-or sequence :key function))
