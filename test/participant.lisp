;;; participant.lisp --- Unit tests for the participant class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.test)

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
