;;;; random.lisp --- Utilities for random testing.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

;;; Generic random utilities

(defun gen-boolean (true &key (true-probability .7))
  (lambda ()
    (when (< (random 1.0) true-probability)
      (funcall true))))

(defun gen-sequence (type length element)
  (lambda ()
    (map-into (make-sequence (funcall type) (funcall length)) element)))

(defmacro define-random-instance-generator ((name class) &body slots)
  (let+ (((&values initargs keyword-parameters)
          (iter (for (name default) :in slots)
                (appending (list (make-keyword name) `(funcall ,name))
                           :into initargs)
                (appending (list (list name default))
                           :into keyword-parameters)
                (finally (return (values initargs keyword-parameters))))))
    `(defun ,name (&key ,@keyword-parameters)
       (lambda () (make-instance ',class ,@initargs)))))

;;; Random scopes

(defparameter *default-random-scope-characters*
  (remove-if-not #'scope-component-character?
                 (mapcar #'code-char (iota 255))))

(defun gen-scope-component
    (&key
     (length    (gen-integer :min 1 :max 1))
     (character (apply #'gen-one-element *default-random-scope-characters*)))
  (gen-string :length length :elements character))

(defun gen-scope (&key
                  (length    (gen-integer :min 1 :max 1))
                  (component (gen-scope-component)))
  (lambda ()
    (rsb:make-scope (funcall (gen-list :length length :elements component)))))

;;; Random participants

(defvar *kinds* '(:listener      :informer
                  :local-method  :local-server
                  :remote-method :remote-server))

(defun gen-kind (&key (kinds *kinds*))
  (gen-one-element kinds))
