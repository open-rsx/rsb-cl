;;; receiving-client.lisp --- Superclass for receiving, filtering clients.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(in-package :rsb)

(defclass receiving-client (participant
			    rsb.ep:client)
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
	(log1 :info participant "Added   filters ~{~S~^, ~}" added)
	(log1 :info participant "Removed filters ~{~S~^, ~}" removed)

	(iter (for filter in added)
	      (rsb.ep:notify configurator filter :filter-added))
	(iter (for filter in removed)
	      (rsb.ep:notify configurator filter :filter-removed))))))

(defmethod print-object ((object receiving-client) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)"
	    (scope-string (participant-scope object))
	    (length (receiver-filters object)))))
