;;; timestamping-receiver-mixin.lisp --- A protocol for receiving and decoding messages.
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

(in-package :rsb.transport)


;;; Mixin class `timestamping-receiver-mixin'
;;

(defclass timestamping-receiver-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into connector classes
that perform two tasks:
+ receive messages
+ decode received messages
The associated protocol is designed to be
direction-agnostic (i.e. should work for both push and pull)."))

(defmethod message->event :around ((connector   timestamping-receiver-mixin)
				   (message     t)
				   (wire-schema t))
  "Add a :receive timestamp to the generated event, if any."
  (let ((event (call-next-method)))
    (when event
      (setf (timestamp event :receive) (local-time:now)))
    event))
