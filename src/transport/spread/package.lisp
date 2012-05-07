;;; package.lisp --- Package definition for transport.spread module.
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

(cl:defpackage :rsb.transport.spread
  (:nicknames :rsb.tp.spread)

  (:shadowing-import-from :rsb.protocol
   :event-id
   :event-meta-data)

  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus

   :nibbles
   
   :rsb
   :rsb.event-processing
   :rsb.transport
   :rsb.protocol)

  ;; Conditions
  (:export
   :assembly-problem
   :assembly-problem-assembly

   :fragment-problem
   :assembly-problem-fragment

   :invalid-fragment-id

   :duplicate-fragment

   :fragmentation-problem

   :insufficient-room
   :fragmentation-problem-required
   :fragmentation-problem-available)

  ;; Variables and constants
  (:export
   :+protocol-version+)
  
  (:documentation
   "This package contains a transport implementation based on the
spread group communication system."))

(cl:in-package :rsb.transport.spread)

(defconstant +protocol-version-base+ 100
  "Constant that has to be subtracted from the field number of the

  .rsb.protocol.Notification.event_id

field to obtain the wire protocol version.")

(defconstant +protocol-version+
  (let ((field-number (pb::field-desc-number
		       (pb:find-descriptor
			".rsb.protocol.Notification.event_id"))))
    (- field-number +protocol-version-base+))
  "Spread wire protocol version. Determined by subtracting the value
of `+protocol-version-base+' from the field number of the

  .rsb.protocol.Notification.event_id

field.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :rsb.transport.spread.num-fragments *transport-metrics*))
