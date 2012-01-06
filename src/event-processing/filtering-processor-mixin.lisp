;;; filtering-processor-mixin.lisp --- Processor for matching events against filters.
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

(in-package :rsb.event-processing)


;;; Filtering processor protocol
;;

(defgeneric processor-filters (processor)
  (:documentation
   "Return the list of filters applied by PROCESSOR."))

(defgeneric (setf processor-filters) (new-value processor)
  (:documentation
   "Set the list of filters applied by PROCESSOR to NEW-VALUE."))


;;; Mixin class `filtering-processor-mixin'
;;

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
