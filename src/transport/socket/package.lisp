;;; package.lisp --- Package definition for the transport.socket module.
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

(cl:defpackage :rsb.transport.socket
  (:nicknames :rsb.tp.sock)

  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsb
   :rsb.event-processing
   :rsb.transport
   :rsb.protocol)

  (:shadowing-import-from :rsb
   :event-id
   :event-meta-data)

  ;; Conditions
  (:export
   :socket-bus-auto-connection-error)

  (:documentation
   "This package contains a transport implementation that uses
multiple point-to-point socket connections to simulate a bus."))
