;;; util.lisp --- Utilities used in the transport.socket module
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

(in-package :rsb.transport.socket)


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


;;; Length reading and writing
;;

(declaim (ftype (function (stream) (unsigned-byte 32)) read-uint32-le)
	 (inline read-uint32-le))

(defun read-uint32-le (stream)
  "Read and return a number encoded as little endian uint32 from STREAM."
  (let ((buffer (binio:make-octet-vector 4)))
    (unless (= (read-sequence buffer stream) 4)
      (error "Short read when receiving notification length."))
    (binio:decode-uint32-le buffer)))

(declaim (ftype (function ((unsigned-byte 32) stream) (values)) write-uint32-le)
	 (inline write-uint32-le))

(defun write-uint32-le (value stream)
  "Write VALUE to STREAM encoded as little endian uint32."
  (let ((buffer (binio:make-octet-vector 4)))
    (binio:encode-uint32-le value buffer)
    (write-sequence buffer stream))
  (values))


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
