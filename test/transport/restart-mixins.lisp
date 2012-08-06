;;; restart-mixins.lisp --- Unit tests for restart mixins.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.transport.test)


;;; Tests for `restart-message-receiver-mixin' class
;;

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
  (message->event simple-connector :does-not-matter :likewise))


;;; Tests for `restart-notification-sender-mixin' class
;;

(deftestsuite restart-notification-sender-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-notification-sender-mixin)))
  (:documentation
   "Unit tests for the `restart-notification-sender-mixin' class."))

(define-restart-method-test-case
    (restart-notification-sender-mixin
     send-notification (connector (notification t)))
    (send-notification simple-connector :ignored))

(define-restart-method-test-case
    (restart-notification-sender-mixin
     event->notification (connector (notification t)))
    (event->notification simple-connector :does-not-matter))
