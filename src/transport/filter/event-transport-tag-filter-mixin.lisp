;;; event-transport-tag-filter-mixin.lisp ---
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

(cl:in-package :rsb.transport.filter)

(defclass event-transport-tag-filter-mixin (transport-tag-filter-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

;;; TODO(jmoringe): proper method for this
(defmethod message->event :around ((connector   event-transport-tag-filter-mixin)
				   (message     t)
				   (wire-schema t))
  "TODO(jmoringe): document"
  (let+ (((&accessors-r/o
	   (better-transports connector-better-transports)) connector))
    (if (not better-transports)
	(call-next-method)
	(when-let ((event (call-next-method)))
	  (when-let ((tags (%extract-transport-tags event)))
	    (log1 :info connector "Processing event with transport tags ~{~S~^, ~}" tags)
	    (iter (for transport in better-transports)
		  (when (member transport tags)
		    (log1 :info connector "Discarding event ~A (transport ~S is preferred)"
			  event transport)
		    (return-from message->event nil))))
	  event))))


;;; Utility functions
;;

(defun %extract-transport-tags (event)
  "Extract and return the list of transports EVENT has been tagged with."
  (mapcar #'make-keyword (split-sequence #\; (meta-data event :transports))))
