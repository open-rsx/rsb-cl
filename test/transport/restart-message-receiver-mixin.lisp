;;; restart-message-receiver-mixin.lisp --- Unit tests for the restart-message-receiver-mixin class.
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

(in-package :rsb.transport.test)

(deftestsuite restart-message-receiver-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-message-receiver-mixin)))
  (:documentation
   "Unit tests for the `restart-message-receiver-mixin' class."))

(define-restart-method-test-case
    (restart-message-receiver-mixin
     receive-message (connector (block? t)))
  (receive-message simple-connector t))

(define-restart-method-test-case
    (restart-message-receiver-mixin
     message->event (connector (message t) (wire-schema t)))
  (message->event simple-connector (make-event "/" "bla") :string))
