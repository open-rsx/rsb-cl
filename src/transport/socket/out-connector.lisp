;;; out-connector.lisp --- Out-direction connector for socket transport.
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

(defmethod find-transport-class ((spec (eql :socket-out)))
  (find-class 'out-connector))

(defclass out-connector (error-handling-sender-mixin
			 restart-notification-sender-mixin
			 connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Out-direction connector for socket transport."))

(defmethod handle ((connector out-connector)
		   (event     event))
  (send-notification connector (event->notification connector event)))

(defmethod event->notification ((connector out-connector)
				(event     event))
  "Delegate conversion to `event->notifications'. The primary purpose
of this method is performing the conversion with restarts installed."
  (event->notification* connector event))

(defmethod send-notification ((connector    out-connector)
			      (notification notification))
  (handle (connector-bus connector) notification))
