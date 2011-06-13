;;; assembly-mixin.lisp ---
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

(defclass assembly-mixin ()
  ((assembly-pool :initarg  :assembly-pool
		  :type     assembly-pool
		  :reader   connector-assembly-pool
		  :documentation
		  ""))
  (:metaclass connector-class)
  (:options
   (:age-limit positive-real
    :default 10
    :description
    "The amount of time after which incomplete assemblies are pruned. Supplying this option only makes sense in conjunction with an unreliable communication mode since incomplete assemblies are never pruned in reliable communication modes."))
  (:documentation
   "This mixin adds an assembly pool and a `notification->event'
method which can be used to assemble incoming notifications (which may
originate from fragmented events) into events."))

(defmethod initialize-instance :after ((instance assembly-mixin)
                                       &key
				       assembly-pool
				       age-limit)
  (unless assembly-pool
    (setf (slot-value instance 'assembly-pool)
	  (if age-limit
	      (make-instance 'pruning-assembly-pool
			     :age-limit age-limit)
	      (make-instance 'assembly-pool)))))
