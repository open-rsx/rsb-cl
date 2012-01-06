;;; connection.lisp --- Unit tests for the connection class.
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

(cl:in-package :rsb.transport.spread.test)

(deftestsuite spread-connection-root (transport-spread-root)
  ()
  (:documentation
   "Unit tests for the `connection' class."))

(addtest (spread-connection-root
          :documentation
	  "Test construction of `connection' instances.")
  construct

  (let ((name (format nil "~D" spread-port)))
    (make-instance 'connection
		   :connection (spread:connect name))
    (make-instance 'connection
		   :name name)

    ;; Neither :connection nor :name => missing required initarg
    (ensure-condition 'missing-required-initarg
      (make-instance 'connection))
    ;; Both :connection and :name => mutually exclusive initargs
    (ensure-condition 'error
      (make-instance 'connection
		     :name       "3333@localhost"
		     :connection (spread:connect name)))))
