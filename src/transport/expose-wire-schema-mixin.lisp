;;; expose-wire-schema-mixin.lisp --- Mixin for connectors that expose a wire-schema
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

(defclass expose-wire-schema-mixin ()
  ((expose-wire-schema? :initarg  :expose-wire-schema?
			:type     boolean
			:reader   connector-expose-wire-schema?
			:initform nil
			:documentation
			"Controls whether the connector should expose
the wire-schemas of received notifications in events constructed form
these notifications."))
  (:metaclass connector-class)
  (:options
   (:expose-wire-schema? &slot))
  (:documentation
   "This class is intended to be mixed into connector classes that
should be able to store the wire-schema of received notifications in
the events constructed from the notifications."))
