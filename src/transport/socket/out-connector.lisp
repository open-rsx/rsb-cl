;;; out-connector.lisp --- Out-direction connector for socket transport.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(in-package :rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-out)))
  (find-class 'out-connector))

(defclass out-connector (error-handling-sender-mixin
			 restart-notification-sender-mixin
			 connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Out-direction connector for socket transport."))

(defmethod handle ((connector out-connector)
		   (event     event))
  (send-notification connector (event->notification connector event)))

(defmethod event->notification ((connector out-connector)
				(event     event))
  "Delegate conversion to `event->notifications'. The primary purpose
of this method is performing the conversion with restarts installed."
  (event->notification* connector event))

(defmethod send-notification ((connector    out-connector)
			      (notification notification))
  (handle (connector-bus connector) notification))
