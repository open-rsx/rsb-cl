;;; in-push-connector.lisp --- In-direction, push-style socket connector.
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

(defmethod find-transport-class ((spec (eql :socket-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (in-connector
			     timestamping-receiver-mixin
			     error-handling-push-receiver-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod handle ((connector in-push-connector)
		   (data      notification))
  ;;; TODO(jmoringe): condition translation?
  (dispatch connector (message->event connector data :undetermined)))
