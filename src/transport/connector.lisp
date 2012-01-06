;;; connector.lisp --- Superclass for connector classes.
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

(in-package :rsb.transport)


;;; `connector' class
;;

(defclass connector (uri-mixin
		     error-policy-mixin)
  ((rsb::uri :reader   connector-url))
  (:documentation
   "A connector implements access to the bus by means of a particular
mechanism. One example is a connector that makes use of the Spread
group communication framework."))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
				     &key
				     schema
				     host
				     port
				     &allow-other-keys)
  (setf (slot-value instance 'rsb::uri)
	(make-instance 'puri:uri
		       :scheme schema
		       :host   host
		       :port   port)))

(defmethod connector-relative-url ((connector connector)
				   (uri       puri:uri))
  (puri:merge-uris uri (connector-url connector)))

(defmethod connector-relative-url ((connector connector)
				   (thing     string))
  (connector-relative-url connector (make-scope thing)))

(defmethod connector-relative-url ((connector connector)
				   (thing     t))
  (connector-relative-url connector (relative-url thing)))

(defmethod print-object ((object connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A ~A"
	    (connector-direction object)
	    (connector-relative-url object "/"))))
