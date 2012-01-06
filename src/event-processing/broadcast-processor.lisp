;;; broadcast-processor.lisp --- Broadcast events to multiple handlers.
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

(defclass broadcast-processor ()
  ((handlers :initarg  :handlers
	     :type     list
	     :accessor handlers
	     :initform nil
	     :documentation
	     ""))
  (:documentation
   "Instances of this class maintain a list of handlers and dispatch
events to these handlers. Methods on `handle' can be use to filter or
transform events. Methods on `dispatch' can be used to modify
dispatching behavior."))

(defmethod handle ((processor broadcast-processor)
		   (data      t))
  "Dispatch DATA to handlers of PROCESSOR in a manner defined by
methods on `dispatch'."
  (dispatch processor data))

(defmethod dispatch ((processor broadcast-processor)
		     (data      t))
  "Dispatch DATA to handlers of PROCESSOR."
  (handle (handlers processor) data))
