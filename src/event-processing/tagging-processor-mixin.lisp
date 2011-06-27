;;; tagging-processor-mixin.lisp --- Processor mixin class for adding tags to events.
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

(defclass tagging-processor-mixin ()
  ((tags :initarg  :tags
	 :type     list
	 :accessor processor-tags
	 :initform nil
	 :documentation
	 "An plist of tags of the form (KEY VALUE ...) that should be
added to events handled by the processor.")
   (tag? :initarg  :tag?
	 :type     boolean
	 :accessor processor-tag?
	 :initform nil
	 :documentation
	 "When non-nil, the processor should perform tagging on
processed events. When nil, the processor should just pass through all
events."))
  (:documentation
   "This class is intended to be mixed into processor classes which
add entries to the meta-data list processed events."))

(defmethod handle :before ((processor tagging-processor-mixin)
			   (event     event))
  "When tagging is enabled in PROCESSOR, add the (tag value)
combination stored in PROCESSOR to the meta-data of EVENT."
  (when (processor-tag? processor)
    (iter (for (key value) on (processor-tags processor) :by #'cddr)
	  (setf (meta-data event key) value))))
