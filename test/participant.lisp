;;;; participant.lisp --- Unit tests for the participant class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.test)

(deftestsuite participant-root (root)
  ()
  (:documentation
   "Test suite for the `participant' class."))

(addtest (participant-root
          :documentation
	  "Test method on `participant-converter' for the
`participant' class.")
  participant-converter

  (ensure-cases (converters query error? expected)
      '((()                             number  nil ())
	(()                             number  t   :error)
	(((number . :a) (integer . :b)) number  nil (:a))
	(((number . :a) (integer . :b)) integer nil (:a :b))
	(((number . :a) (integer . :b)) string  nil ())
	(((number . :a) (integer . :b)) string  t   :error))

    (let ((participant (make-instance 'participant
				      :scope      "/"
				      :converters converters)))
      (if (eq expected :error)
	  (ensure-condition 'error
	    (participant-converter participant query
				   :error? error?))
	  (let ((result (participant-converter participant query
					       :error? error?)))
	    (ensure-same result expected
			 :test #'set-equal))))))
