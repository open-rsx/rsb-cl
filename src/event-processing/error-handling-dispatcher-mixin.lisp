;;; error-handling-dispatcher-mixin.lisp --- A mixin for dispatch error handling.
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

(cl:in-package :rsb.event-processing)

(defclass error-handling-dispatcher-mixin (error-policy-mixin)
  ()
  (:documentation
   "This mixin class is intended to mixed into processor classes that
perform potentially error signaling tasks in their `dispatch'
methods. This class adds an :around method on `dispatch' that installs
restarts for error recovery and optionally calls an error policy
function."))

(defmethod dispatch :around ((processor error-handling-dispatcher-mixin)
			     (event     event))
  "Install log and ignore restarts around a call to the next
`dispatch' method. In case of an error, call the error-policy function
of PROCESSOR, if any."
  (with-error-policy (processor)
    (restart-case
	(call-next-method)
      (log (&optional condition)
	:report (lambda (stream)
		  (format stream "~@<Log a message and ignore the ~
failure to dispatch event ~A.~@:>"
			  event))
	(log1 :warn processor "Failed to dispatch the event ~A~@[: ~A~]" event condition)
	nil)
      (continue ()
	:report (lambda (stream)
		  (format stream "~@<Ignore the failure to dispatch ~
event ~A.~@:>"
			  event))
	nil))))
