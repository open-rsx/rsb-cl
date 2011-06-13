;;; message-receiver-mixin.lisp --- A protocol for receiving and decoding messages.
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

(in-package :rsb.transport)


;;; Message receiver protocol
;;

(defgeneric receive-message (connector block?)
  (:documentation
   "Receive and return one XMPP message."))

(defgeneric message->event (connector message wire-schema)
  (:documentation
   "Convert MESSAGE with wire-schema WIRE-SCHEMA into an `event'
instance and return the event. If message cannot be converted into an
event, return nil instead."))


;;; Mixin class `message-receiver-mixin'
;;

(defclass message-receiver-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into connector classes
that perform two tasks:
+ receive messages
+ decode received messages
The associated protocol is designed to be
direction-agnostic (i.e. should work for both push and pull)."))
