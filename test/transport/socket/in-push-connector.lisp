;;; in-push-connector.lisp --- Unit tests for the in-push-connector class
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

(deftestsuite in-push-connector-root (transport-socket-root
				      connector-suite)
  ()
  (:documentation
   "Test suite for the `in-push-connector' class."))

(define-basic-connector-test-cases in-push-connector
    :expected-direction :in-push
    :expected-wire-type 'octet-vector
    :expected-schemas   '(:socket)
    :construct-args     (:host      "localhost"
			 :port      *next-port*
			 :converter :fundamental-null))

(addtest (in-push-connector-root
          :documentation
	  "Test constructing `in-push-connector' instances.")
  construction

  (ensure-condition 'missing-required-initarg
    (make-instance 'in-push-connector)))
