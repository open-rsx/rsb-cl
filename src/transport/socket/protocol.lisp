;;; protocol.lisp --- Protocol used in the socket transport module.
;;
;; Copyright (C) 2013 Jan Moringen
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


;;; Connection shutdown protocol
;;

(defgeneric shutdown-handshake-for (condition)
  (:documentation
   "Return a keyword indicating the appropriate kind of shutdown
handshake to perform for CONDITION."))

(defmethod shutdown-handshake-for ((condition error))
  "Default behavior consists in not performing a shutdown handshake
when encountering an error condition."
  nil)

(defgeneric disconnect (connection
			&key
			abort
			handshake)
  (:documentation
   "Maybe perform shutdown handshake, stop receiver thread and close
socket of connection.

When ABORT is non-nil, skip the shutdown handshake unconditionally.

HANDSHAKE can be either nil, :send or :receive."))
