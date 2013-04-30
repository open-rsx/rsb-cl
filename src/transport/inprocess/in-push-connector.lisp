;;; in-push-connector.lisp ---
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (connector
			     broadcast-processor
			     error-handling-dispatcher-mixin
			     error-handling-push-receiver-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod notify ((connector in-push-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (log1 :info connector "Attaching to scope ~S" scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-push-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (log1 :info connector "Detaching from scope ~S" scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle :before ((connector in-push-connector)
			   (event     event))
  (setf (timestamp event :receive) (local-time:now)))
