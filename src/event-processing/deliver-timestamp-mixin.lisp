;;; deliver-timestamp-mixin.lisp --- A mixin that adds :deliver timestamps.
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

(defclass deliver-timestamp-mixin ()
  ()
  (:documentation
   "This class can be mixed into event processor classes that used in
event delivery contexts to attach :deliver timestamps to processed
events."))

(defmethod handle :before ((processor deliver-timestamp-mixin)
			   (event     event))
  "Attach a :deliver timestamp to EVENT."
  (setf (timestamp event :deliver) (local-time:now)))
