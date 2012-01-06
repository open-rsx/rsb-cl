;;; in-pull-connector.lisp --- Unit tests for the inprocess:in-pull-connector class.
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

(in-package :rsb.transport.inprocess.test)

(deftestsuite in-pull-connector-root (transport-inprocess-root
				      connector-suite)
  ()
  (:documentation
   "Root unit tests suite for the `in-pull-connector' class."))

(define-basic-connector-test-cases in-pull-connector
    :expected-direction :in-pull
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
