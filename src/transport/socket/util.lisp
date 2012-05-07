;;; util.lisp --- Utilities used in the transport.socket module
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


;;; Connection options
;;

(defun make-connection-options (connector)
  "Return a plist of connection options that should be used by
connections associated to CONNECTOR."
  (list :nodelay? (connector-nodelay? connector)))

(defun check-connection-options (bus-options options)
  "Signal an error if the option plists BUS-OPTIONS and OPTIONS
contain conflicting properties."
  (with-simple-restart (continue "~@<Ignore the incompatibility and ~
use the existing bus object.~@:>")
    (iter (for (key value) on options :by #'cddr)
	  (let ((bus-value (getf bus-options key)))
	    (unless (equalp bus-value value)
	      (error "~@<Incompatible values for option ~S: current ~
bus uses value ~S; requested value is ~S.~@:>"
		     key bus-value value))))))


;;; Printing utility functions
;;

(defun print-socket (stream socket &optional colon? at?)
  "Print ENDPOINT to STREAM."
  (declare (ignore at?))
  (format stream "~:[ADDRESS?~;~:*~A~]:~:[PORT?~;~:*~D~]"
	  (ignore-errors
	    (if colon?
		(usocket:get-local-name socket)
		(usocket:get-peer-name  socket)))
	  (ignore-errors
	    (if colon?
		(usocket:get-local-port socket)
		(usocket:get-peer-port  socket)))))
