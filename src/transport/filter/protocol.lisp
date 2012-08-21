;;; protocol.lisp --- Protocol for the transport.filter module.
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


;;; Notification filter construction protocol
;;

(defgeneric make-notification-filter-for-filter (connector filter)
  (:documentation
   "Make and return a filter instance that implements the behavior of
FILTER by matching notifications received by CONNECTOR or nil if such
a filter cannot be constructed."))


;;; Default behavior
;;

(defmethod make-notification-filter-for-filter ((connector t)
						(filter    t))
  "Default behavior is to not return a filter."
  (values))
