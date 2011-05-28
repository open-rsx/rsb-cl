;;; filtering-processor-mixin.lisp --- Processor for matching events against filters.
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

(in-package :rsb.event-processing)

(defclass filtering-processor-mixin ()
  ((filter :initarg  :filter
	   :type     rsb.filter:conjoin-filter
	   :reader   processor-filter
	   :initform (make-instance 'rsb.filter:conjoin-filter)
	   :documentation
	   "The filter object is used to hold a list of filters that
act in a conjunctive manner."))
  (:documentation
   "The mixin class adds filtering of events before further
processing. Processors of this kind maintain a list of filters that
can be accessed and modified using the `process-filters'
accessor. Events are dropped in a method on `handle' before any
further processing unless they match all associated filters of the
respective processor."))

(defmethod processor-filters ((processor filtering-processor-mixin))
  (rsb.filter:filter-children (processor-filter processor)))

(defmethod (setf processor-filters) ((new-value list)
				     (processor filtering-processor-mixin))
  (setf (rsb.filter:filter-children (processor-filter processor))
	new-value))

(defmethod handle ((processor filtering-processor-mixin)
		   (event     event))
  "Terminate processing of EVENT unless it matches PROCESSOR's
filters."
  (when (rsb.filter:matches? (processor-filter processor) event)
    (call-next-method)))
