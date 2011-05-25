;;; in-push-connector.lisp ---
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

(in-package :rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (connector
			     broadcast-processor)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod notify ((connector in-push-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (log1 :info "~S Attaching to scope ~S" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-push-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (log1 :info "~S detaching from scope ~S" connector scope)
  (removef (by-scope scope) connector :count 1))
