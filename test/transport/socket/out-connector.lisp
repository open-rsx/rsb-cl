;;; out-connector.lisp --- Unit tests for the out-connector class
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

(cl:in-package :rsb.transport.socket.test)

(deftestsuite out-connector-root (transport-socket-root
				  connector-suite)
  ()
  (:documentation
   "Test suite for the `out-connector' class."))

(define-basic-connector-test-cases out-connector
    :expected-direction :out
    :expected-wire-type 'octet-vector
    :expected-schemas   '(:socket)
    :construct-args     (:host      "localhost"
			 :port      *next-port*
			 :converter :fundamental-null))

(addtest (out-connector-root
          :documentation
	  "Test constructing `out-connector' instances.")
  construction

  (ensure-condition 'missing-required-initarg
    (make-instance 'out-connector)))
