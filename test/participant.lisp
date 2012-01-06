;;; participant.lisp --- Unit tests for the participant class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
