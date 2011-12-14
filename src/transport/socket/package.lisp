;;; package.lisp --- Package definition for the transport.socket module.
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

(cl:defpackage :rsb.transport.socket
  (:nicknames :rsb.tp.sock)

  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsb
   :rsb.event-processing
   :rsb.transport
   :rsb.protocol)

  (:shadowing-import-from :rsb
   :event-id
   :event-meta-data)

  ;; Conditions
  (:export
   :socket-bus-auto-connection-error)

  (:documentation
   "This package contains a transport implementation that uses
multiple point-to-point socket connections to simulate a bus."))
