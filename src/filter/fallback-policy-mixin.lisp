;;; fallback-policy-mixin.lisp --- A fallback policy for partial filters.
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

(cl:in-package :rsb.filter)

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
  (let+ (((&accessors-r/o
	   (fallback-policy filter-fallback-policy)) filter))
    (ecase fallback-policy
      (:match        t)
      (:do-not-match nil))))

(defmethod print-object ((object fallback-policy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-fallback-policy object))))
