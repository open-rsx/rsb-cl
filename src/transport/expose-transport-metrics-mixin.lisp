;;; expose-transport-metrics-mixin.lisp --- Mixin for connectors that expose transport metrics.
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

(defclass expose-transport-metrics-mixin ()
  ((expose :initarg  :expose
	   :accessor connector-expose
	   :initform nil
	   :documentation
	   "Controls which metrics of received notifications the
connector should expose in events constructed from these
notifications."))
  (:metaclass connector-class)
  (:options
   (:expose &slot))
  (:documentation
   "This class is intended to be mixed into connector classes that
should be able to store transport metrics of received notifications in
the events constructed from the notifications."))

(defmethod shared-initialize :after ((instance   expose-transport-metrics-mixin)
                                     (slot-names t)
                                     &key
				     (expose nil expose-supplied?))
  (when expose-supplied?
    (setf (connector-expose instance) expose)))

(defmethod connector-expose? ((connector expose-transport-metrics-mixin)
			      (metric    symbol))
  (declare (notinline member))
  (member metric (connector-expose connector) :test #'eq))

(defmethod (setf connector-expose?) ((new-value (eql nil))
				     (connector expose-transport-metrics-mixin)
				     (metric    symbol))
  (removef (connector-expose connector) metric)
  new-value)

(defmethod (setf connector-expose?) ((new-value t)
				     (connector expose-transport-metrics-mixin)
				     (metric    symbol))
  (pushnew metric (connector-expose connector))
  new-value)

(defmethod (setf connector-expose) ((new-value symbol)
				    (connector expose-transport-metrics-mixin))
  (setf (connector-expose connector) (list new-value))
  new-value)
