;;; transport-tag-filter-mixin.lisp --- A transport-level filter for transport list tags.
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

(cl:in-package :rsb.transport.filter)

(defclass transport-tag-filter-mixin ()
  ((better-transports :initarg  :better-transports
		      :type     list
		      :accessor connector-better-transports
		      :initform nil
		      :documentation
		      "This slot stores a list of transport names (as
in `connector-schemas') of transports which
1. provide alternate routes to ourselves
2. are preferred over us by policy"))
  (:documentation
   "This class is intended to be mixed into connector classes which
have to keep track of transport tag filters. This class add the
management of a list of preferred transports (preferred over the
connector itself) based on added and removed `transport-tag-filter'
instances. It is assumed that only a single `transport-tag-filter' is
in effect at a time."))

(defmethod notify ((connector transport-tag-filter-mixin)
		   (filter    transport-tag-filter)
		   (action    (eql :filter-added)))
  "When CONNECTOR is notified of an added `transport-tag-filter'
FILTER, it extracts the transport ranking and computes preferred
transports."
  (log1 :info connector "Was notified of added filter ~S with ranking ~{~S~^ ⋨ ~}."
	filter (filter-transport-ranking filter))

  ;; Construct the set of transports in the transport ranking of
  ;; FILTER which preferred over the transport implemented by
  ;; CONNECTOR. Store this set for matching.
  (let* ((ranking (filter-transport-ranking filter))
	 (better  (compute-better-transports ranking connector))) ;;; TODO(jmoringe): warn if we are not in ranking?
    (log1 :info connector "Preferred transports: ~{~S~^, ~}" better)
    (setf (connector-better-transports connector) better)))

(defmethod notify ((connector transport-tag-filter-mixin)
		   (filter    transport-tag-filter)
		   (action    (eql :filter-removed)))
  "When CONNECTOR is notified of an removed `transport-tag-filter'
FILTER, resets its list of preferred transports."
  (log1 :info connector "Was notified of removed filter ~S." filter)
  (setf (connector-better-transports connector) nil))


;;; Utility functions
;;

(defun %compute-better-transports (ranking connector)
  "Compute and return the list of transports in RANKING that should be
preferred over the transport used by CONNECTOR."
  (if-let ((index (position (first (connector-schemas connector)) ranking)))
    (subseq ranking 0 index)
    ranking))

;; Local Variables:
;; coding: utf-8
;; End:
