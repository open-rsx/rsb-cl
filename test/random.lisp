;;;; random.lisp --- Utilities for random testing.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

;;; Generic random utilities

(defun random-boolean (true &key (true-probability .7))
  (lambda ()
    (when (< (random 1.0) true-probability)
      (funcall true))))

(defun random-integer (min max)
  (if (= min max)
      (constantly min)
      (lambda ()
        (+ min (random (1+ (- max min)))))))

(defun random-element (sequence)
  (curry #'random-elt sequence))

(defun random-sequence (type length element)
  (lambda ()
    (map-into (make-sequence (funcall type) (funcall length)) element)))

(defparameter *default-random-characters*
  (remove-if-not #'alphanumericp (mapcar #'code-char (iota 255))))

(defun random-character (&key (characters *default-random-characters*))
  (random-element characters))

(defun random-string (length &key (character (random-character)))
  (random-sequence (constantly 'string) length character))

(defun random-list (length element)
  (random-sequence (constantly 'list) length element))

(defmacro define-random-instance-maker ((name class) &body slots)
  (let+ (((&values initargs keyword-parameters)
          (iter (for (name default) :in slots)
                (appending (list (make-keyword name) `(funcall ,name)) :into initargs)
                (appending (list (list name default)) :into keyword-parameters)
                (finally (return (values initargs keyword-parameters))))))
    `(defun ,name (&key ,@keyword-parameters)
       (lambda () (make-instance ',class ,@initargs)))))

;;; Random scopes

(defparameter *default-random-scope-characters*
  (remove-if-not #'scope-component-character?
                 (mapcar #'code-char (iota 255))))

(defun random-scope-component
    (&key
     (length    (random-integer 1 1))
     (character (random-character
                 :characters *default-random-scope-characters*)))
  (random-string length :character character))

(defun random-scope (&key
                     (length    (random-integer 1 1))
                     (component (random-scope-component)))
  (lambda ()
    (rsb:make-scope (funcall (random-list length component)))))

;;; Random participants

(defvar *kinds* '(:listener      :informer
                  :local-method  :local-server
                  :remote-method :remote-server))

(defun random-kind (&key (kinds *kinds*))
  (random-element kinds))
