;;; conditions.lisp --- Conditions used in the socket transport.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(define-condition socket-bus-auto-connection-error (rsb-error
						    simple-error)
  ()
  (:default-initargs
   :references (append
		(default-references 'rsb-error)
		(list (documentation-ref/rsb-manual
		       "Troubleshooting"
		       "Configuring the TCP-based Transport"))))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to socket-based bus as ~
automatically determined client or server~@:>")
     (maybe-print-explanation stream condition)))
  (:documentation
   "This error is signaled when a an attempt to obtain a bus
provider in automatic client vs. server selection mode."))
