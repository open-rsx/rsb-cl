;;; error-handling-mixins.lisp --- Error handling mixins for connector classes.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
