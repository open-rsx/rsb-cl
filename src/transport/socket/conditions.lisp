;;; conditions.lisp --- Conditions used in the socket transport.
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

(define-condition socket-bus-auto-connection-error (rsb-error
						    simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to socket-based bus as ~
automatically determined client or server~@:>")
     (maybe-print-explanation stream condition)))
  (:documentation
   "This error is signaled when a an attempt to obtain a bus
provider in automatic client vs. server selection mode."))
