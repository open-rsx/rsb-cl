;;; package.lisp --- Package definition for the transport.inprocess module.
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

(cl:defpackage :rsb.transport.inprocess
  (:nicknames
   :rsb.tp.inprocess
   :rsb.tp.inproc)

  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus

   :rsb
   :rsb.event-processing
   :rsb.filter
   :rsb.transport)

  ;; Exported for unit tests
  (:export
   :in-pull-connector
   :in-push-connector
   :out-connector)

  (:documentation
   "This package contains a transport that delivers RSB events within
a process."))
