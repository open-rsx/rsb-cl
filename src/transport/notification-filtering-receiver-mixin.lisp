;;; notification-filtering-receiver-mixin.lisp --- Notification filtering mixin for connector classes.
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

(cl:in-package :rsb.transport)


;;; Mixin class `notification-filtering-receive-mixin'
;;

(defclass notification-filtering-receiver-mixin ()
  ((notification-filter :reader   connector-notification-filter
			:initform (make-instance 'rsb.filter:conjoin-filter)
			:documentation
			"This filter instance is used to store the set
of filters applied by the connector to incoming notifications."))
  (:documentation
   "This class is intended to be mixed into connector classes that
perform filtering of notifications. Instances maintain a list of
filters incoming notifications have to in order to not be
discarded."))

(defmethod connector-notification-filters ((connector notification-filtering-receiver-mixin))
  (filter-children (connector-notification-filter connector)))

(defmethod (setf connector-notification-filters) ((new-value list)
						  (connector notification-filtering-receiver-mixin))
  (setf (filter-children (connector-notification-filter connector))
	new-value))

(defmethod matches? ((connector    notification-filtering-receiver-mixin)
		     (notification t))
  (matches? (connector-notification-filter connector) notification))

(defmethod receive-message :around ((connector notification-filtering-receiver-mixin)
				    (block?    t))
  "Call the next method to receive notifications. Discard
notifications which do not match the set of filters installed on
CONNECTOR."
  (let (notification)
    (iter (setf notification (call-next-method))
	  (unless (matches? connector notification)
	    (setf notification nil))
	  (until (or notification (not block?))))
    notification))
