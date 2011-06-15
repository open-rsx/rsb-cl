;;; deliver-timestamp-mixin.lisp --- A mixin that adds :deliver timestamps.
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

(defclass deliver-timestamp-mixin ()
  ()
  (:documentation
   "This class can be mixed into event processor classes that used in
event delivery contexts to attach :deliver timestamps to processed
events."))

(defmethod dispatch :before ((processor deliver-timestamp-mixin)
			     (event     event))
  "Attach a :deliver timestamp to EVENT."
  (setf (timestamp event :deliver) (local-time:now)))
