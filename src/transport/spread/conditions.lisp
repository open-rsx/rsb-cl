;;; conditions.lisp --- Conditions used in the spread transport implementation.
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

(cl:in-package :rsb.transport.spread)


;;; Assembly-related conditions
;;

(define-condition assembly-problem (condition)
  ((assembly :initarg  :assembly
	     :reader   assembly-problem-assembly
	     :documentation
	     "The assembly instance in which the problem occurred."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Something happened in assembly ~A.~@:>"
	     (assembly-problem-assembly condition))))
  (:documentation
   "Instance of subclasses of this condition are signaled if a problem
occurs during the assembly of fragments into complete events."))

(define-condition fragment-problem (assembly-problem)
  ((fragment :initarg  :fragment
	     :reader   assembly-problem-fragment
	     :documentation
	     "The notification representing the fragment that caused
the problem."))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
      (format stream "~@<The fragment ~D of event ~/rsb::print-id/ ~
caused a problem in assembly ~A.~@:>"
	      (rsb.protocol::fragmented-notification-data-part fragment)
	      (assembly-id assembly)
	      assembly))))
  (:documentation
   "Instance of subclasses of this condition are signaled if a
fragment causes a problem in an assembly."))

(define-condition invalid-fragment-id (fragment-problem
				       warning)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
       (format stream "~@<Received illegal fragment ~D of event ~
~/rsb::print-id/ with ~D parts in assembly ~A.~@:>"
	       (rsb.protocol::fragmented-notification-data-part fragment)
	       (assembly-id assembly)
	       (length (assembly-fragments assembly))
	       assembly))))
  (:documentation
   "This warning is signaled when an attempt is made to add a fragment
with an invalid part id to an assembly."))

(define-condition duplicate-fragment (fragment-problem
				      warning)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
       (format stream "~@<Received fragment ~D of event ~
~/rsb::print-id/ more than once in assembly ~A.~@:>"
	       (rsb.protocol::fragmented-notification-data-part fragment)
	       (assembly-id assembly)
	       assembly))))
  (:documentation
   "This warning is signaled when an attempt is made to add a fragment
to an assembly that has already been added."))


;;; Fragmentation-related conditions
;;

(define-condition fragmentation-problem (condition)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<A fragmentation operation failed~@:>")))
  (:documentation
   "Conditions of this class and subclasses are signaled when problems
related to fragmenting events into multiple notifications are
encountered."))

(define-condition insufficient-room (fragmentation-problem
				     error)
  ((required  :initarg  :required
	      :type     positive-integer
	      :reader   fragmentation-problem-required
	      :documentation
	      "")
   (available :initarg  :available
	      :type     non-negative-integer
	      :reader   fragmentation-problem-available
	      :documentation
	      ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Insufficient room (~:D byte~:P) for fragment ~
requiring ~:D byte~:P.~@:>"
	     (fragmentation-problem-available condition)
	     (fragmentation-problem-required  condition))))
  (:documentation
   "This error is signaled when a notification fragment of an event
cannot be created because it would exceed the maximum allowed fragment
size."))
