;;;; protocol.lisp --- Unit tests for the protocol provided by the event-processing module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing.test)

(deftestsuite rsb.event-processing.transform!-root (event-processing-root)
  ()
  (:documentation
   "Unit tests for the `transform!' generic function."))

(addtest (rsb.event-processing.transform!-root
          :documentation
	  "Test default behavior of the `transform!' generic
function.")
  default-behavior

  (ensure-cases (transform object expected)
      `(;; Some invalid transforms.
	(:no-such-transform     :does-not-matter transform-error)
	((:no-such-transform)   :does-not-matter transform-error)
	(,#'1+                  :wrong-type      transform-error)

	;; These are valid
	(,#'1+                  1                2)
	((,#'1+ ,(curry #'* 2)) 1                3)
	((,(curry #'* 2) ,#'1+) 1                4))

    (let+ (((&flet do-it ()
	      (transform! transform object))))
      (case expected
	(transform-error (ensure-condition 'transform-error (do-it)))
	(t               (ensure-same (do-it) expected))))))

(addtest (rsb.event-processing.transform!-root
          :documentation
	  "Test restarts established by default methods on the
`transform!' generic function.")
  restarts

  (ensure-cases (restart expected)
      '((continue      1)
	((use-value 2) 2))

      (handler-bind
	  ((error (lambda (condition)
		    (let+ (((name &rest args) (ensure-list restart))
			   (restart (find-restart name)))
		      (ensure restart)          ; ensure it is there
		      (princ-to-string restart) ; ensure it prints
		      (apply #'invoke-restart restart args)))))
	(ensure-same
	 (transform! (curry #'error "~@<I hate ~A~@:>") 1)
	 expected))))
