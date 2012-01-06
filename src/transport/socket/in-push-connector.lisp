;;; in-push-connector.lisp --- In-direction, push-style socket connector.
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

(cl:in-package :rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (in-connector
			     timestamping-receiver-mixin
			     error-handling-push-receiver-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod handle ((connector in-push-connector)
		   (data      notification))
  ;;; TODO(jmoringe): condition translation?
  (dispatch connector (message->event connector data :undetermined)))
