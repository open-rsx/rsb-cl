;;; error-handling-mixins.lisp --- Error handling mixins for connector classes.
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


;;; Mixin class `error-handling-pull-receiver-mixin'
;;

(defclass error-handling-pull-receiver-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into in-direction, pull-style
connector classes to provide client-supplied error handling policies
for the `emit' method."))

(defmethod emit :around ((connector error-handling-pull-receiver-mixin)
			 (block?    t))
  "Call the actual `emit' method with a condition handler that applies
the error policy of CONNECTOR."
  (with-error-policy (connector)
    (call-next-method)))


;;; Mixin class `error-handling-push-receiver-mixin'
;;

(defclass error-handling-push-receiver-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into in-direction, push-style
connector classes to provide client-supplied error handling policies
for the `receive-messages' method."))

(defmethod receive-messages :around ((connector error-handling-push-receiver-mixin))
  "Call the actual `receive-messages' method with a condition handler
that applies the error policy of CONNECTOR."
  (with-error-policy (connector)
    (call-next-method)))


;;; Mixin class `error-handling-sender-mixin'
;;

(defclass error-handling-sender-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into out-direction connector
classes to provide client-supplied error handling policies for the
`handle' method."))

(defmethod handle :around ((connector error-handling-sender-mixin)
			   (event     event))
  "Call the actual `handle' method with a condition handler that
applies the error policy of CONNECTOR."
  (with-error-policy (connector)
    (call-next-method)))
