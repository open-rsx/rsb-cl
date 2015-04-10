;;;; reloading.lisp --- Code to be executed at startup/shutdown.
;;;;
;;;; Copyright (C) 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;; Reseed the `random-state' stored in `*id-random-state*'.
;;
;; This code ensures that `*id-random-state*' is reseeded at image
;; startup thereby ensuring different pseudo random ids and such for
;; subsequent program runs.
(defun reseed-id-random-state ()
  (setf *id-random-state* (make-random-state t)))

(uiop:register-image-restore-hook 'reseed-id-random-state nil)
