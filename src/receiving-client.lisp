;;; receiving-client.lisp --- Superclass for receiving, filtering clients.
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

(in-package :rsb)

(defclass receiving-client (rsb.ep:client)
  ((filters :type     list
	    :initform nil
	    :accessor receiver-filters
	    :documentation
	    "The list of filters that events have to match in order to
be received by the participant."))
  (:documentation
   "This class is intended to be used as a superclass of event
processing configuration clients that receive and filter events."))

(defmethod (setf receiver-filters) :around ((new-value   list)
					    (participant receiving-client))
  "Notify interested parties of the change in the set of
listeners."
  (bind (((:accessors-r/o
	   (old-value    receiver-filters)
	   (configurator rsb.ep:client-configurator)) participant))
    (prog1
	(call-next-method)
      (let ((added   (set-difference new-value old-value))
	    (removed (set-difference old-value new-value)))
	(log1 :info "~S added   filters ~{~S~^, ~}" participant added)
	(log1 :info "~S removed filters ~{~S~^, ~}" participant removed)

	(iter (for filter in added)
	      (rsb.ep:notify configurator filter :filter-added))
	(iter (for filter in removed)
	      (rsb.ep:notify configurator filter :filter-removed))))))

(defmethod print-object ((object receiving-client) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)"
	    (scope-string (participant-scope object))
	    (receiver-filters object))))
