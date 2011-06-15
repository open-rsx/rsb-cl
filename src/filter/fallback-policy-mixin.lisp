;;; fallback-policy-mixin.lisp --- A fallback policy for partial filters.
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

(in-package :rsb.filter)

(defmethod find-filter-class ((spec (eql :constant)))
  (find-class 'fallback-policy-mixin))

(defclass fallback-policy-mixin ()
  ((fallback-policy :initarg  :always
		    :initarg  :fallback-policy
		    :type     fallback-policy
		    :accessor filter-fallback-policy
		    :initform :match
		    :documentation
		    "The value of this slots determines the behavior
of the filter in case it primary discrimination mechanism is not
applicable to an event."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
cannot process all events using their primary discrimination method
and thus need a fallback policy."))

(defmethod matches? ((filter fallback-policy-mixin)
		     (event  t))
  "Decide whether EVENT should match FILTER based on FILTER's fallback
policy. This method is only called, if no more specific method on
`matches?' made a decision."
  (bind (((:accessors-r/o
	   (fallback-policy filter-fallback-policy)) filter))
    (case fallback-policy
      (:match        t)
      (:do-not-match nil))))

(defmethod print-object ((object fallback-policy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-fallback-policy object))))
