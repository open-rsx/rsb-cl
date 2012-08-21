;;; event-filter->notification-filter-mixin.lisp --- Management of notification filters.
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


;;; Mixin class `event-filter->notification-filter-mixin'
;;

(defclass event-filter->notification-filter-mixin (notification-filtering-receiver-mixin)
  ((filter-mapping :initarg  :filter-mapping
		   :type     list
		   :accessor connector-filter-mapping
		   :initform nil
		   :documentation
		   "Stores an alist of event filters and corresponding
notification filters."))
  (:documentation
   "This class is intended to mixed into connector classes that
perform notification filtering using filters that are derived from
client-supplied event filters. A method on
`connector-notification-filters' (and the associated `setf'-method)
has to be defined."))

(defmethod notify ((connector event-filter->notification-filter-mixin)
		   (filter    t)
		   (action    (eql :filter-added)))
  (log1 :info connector "Was notified of added filter ~S" filter)

  (let+ (((&accessors
	   (filter-mapping       connector-filter-mapping)
	   (notification-filters connector-notification-filters)) connector)
	 (notification-filter (make-notification-filter-for-filter
			       connector filter)))
    (if notification-filter
	(progn
	  (push (cons filter notification-filter) filter-mapping)
	  (push notification-filter notification-filters)
	  (log1 :info connector "Implemented ~S using ~S" filter notification-filter)
	  :implemented)
	:not-implemented)))

(defmethod notify ((connector event-filter->notification-filter-mixin)
		   (filter    t)
		   (action    (eql :filter-removed)))
  (log1 :info connector "Was notified of removed filter ~S" filter)

  (let+ (((&accessors
	   (filter-mapping       connector-filter-mapping)
	   (notification-filters connector-notification-filters)) connector)
	 (entry (find filter filter-mapping :key #'car)))
    (when entry
      (removef notification-filters (cdr entry))
      (removef filter-mapping entry)
      (log1 :info connector "Removed filter ~S" (cdr entry))))

  :implemented)
