;;;; reloading.lisp --- Code to be executed at startup/shutdown.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb)

(defun enable-id-random-state-reseed ()
  "Reseed the `random-state' stored in `*id-random-state*'.

This function can be called to ensure that `*id-random-state*' is
reseeded at image startup thereby ensuring different pseudo random ids
and such for subsequent program runs."
  #+sbcl
  (push (lambda () (setf *id-random-state* (make-random-state t)))
        sb-ext:*init-hooks*))
