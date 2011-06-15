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
  ()
  (:documentation
   "This class is intended to be used as a superclass of event
processing configuration clients that receive and filter events."))

(defmethod receiver-filters ((participant receiving-client))
  (rsb.ep:configurator-filters (rsb.ep:client-configurator participant)))

(defmethod (setf receiver-filters) ((new-value   list)
				    (participant receiving-client))
  (setf (rsb.ep:configurator-filters
	 (rsb.ep:client-configurator participant))
	new-value))

(defmethod print-object ((object receiving-client) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)"
	    (scope-string (participant-scope object))
	    (receiver-filters object))))
