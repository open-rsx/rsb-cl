;;; assembly-mixin.lisp ---
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

(in-package :rsb.transport.spread)

(defclass assembly-mixin ()
  ((assembly-pool :initarg  :assembly-pool
		  :type     assembly-pool
		  :reader   connector-assembly-pool
		  :documentation
		  ""))
  (:metaclass connector-class)
  (:options
   (:age-limit positive-real
    :default 10
    :description
    "The amount of time after which incomplete assemblies are pruned. Supplying this option only makes sense in conjunction with an unreliable communication mode since incomplete assemblies are never pruned in reliable communication modes."))
  (:documentation
   "This mixin adds an assembly pool and a `notification->event'
method which can be used to assemble incoming notifications (which may
originate from fragmented events) into events."))

(defmethod initialize-instance :after ((instance assembly-mixin)
                                       &key
				       assembly-pool
				       age-limit)
  (unless assembly-pool
    (setf (slot-value instance 'assembly-pool)
	  (if age-limit
	      (make-instance 'pruning-assembly-pool
			     :age-limit age-limit)
	      (make-instance 'assembly-pool)))))
