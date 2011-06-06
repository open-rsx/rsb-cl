;;; in-connector.lisp --- Superclass for in-direction connector classes.
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

(in-package :rsb.transport.spread)


;;; `in-connector' class
;;

(defclass in-connector (connector
			broadcast-processor
			assembly-mixin
			conversion-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
connector classes for Spread."))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope))) ;; TODO pass a private/thread-local scope cache?
    (ref-group connection group-name))
  :implemented)

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope)))
    (unref-group connection group-name))
  :implemented)
