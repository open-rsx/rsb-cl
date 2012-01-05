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
   :bind

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

  (:documentation
   "This package contains a transport implementation based on the
spread group communication system."))
