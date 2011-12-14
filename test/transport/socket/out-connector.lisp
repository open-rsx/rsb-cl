;;; out-connector.lisp --- Unit tests for the out-connector class
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.transport.socket.test)

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
