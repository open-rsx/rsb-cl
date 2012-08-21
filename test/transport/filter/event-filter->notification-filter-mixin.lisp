;;; event-filter->notification-filter-mixin.lisp --- Unit tests for the event-filter->notification-filter-mixin class.
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

(cl:in-package :rsb.transport.filter.test)


;;; Class `mock-connector'
;;

(defclass mock-connector (event-filter->notification-filter-mixin)
  ())

(defmethod make-notification-filter-for-filter ((connector mock-connector)
						(filter    origin-filter))
  (make-instance 'rsb.filter::type-filter
		 :type (list 'eql (filter-origin filter))))


;;; Tests
;;

(deftestsuite event-filter->notification-filter-mixin-root (transport-filter-root)
  ((simple-connector (make-instance 'mock-connector))
   (simple-filter    (filter :origin :origin (uuid:make-v1-uuid))))
  (:documentation
   "Unit tests for the `event-filter->notification-filter-mixin'
class."))

(addtest (event-filter->notification-filter-mixin-root
          :documentation
	  "Smoke test for the
`event-filter->notification-filter-mixin' class.")  smoke

  ;; After notifying the connector of an added filter, there should be
  ;; one notification filter.
  (notify simple-connector simple-filter :filter-added)
  (ensure (connector-notification-filters simple-connector))

  ;; After notifying the connector about the previously added filter
  ;; having been removed, there should be no notification filters.
  (notify simple-connector simple-filter :filter-removed)
  (ensure-null (connector-notification-filters simple-connector)))
