;;; conditions.lisp --- Conditions used in the filter module of cl-rsb.
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

(in-package :rsb.filter)

(define-condition filter-construction-error (rsb-error
					     chainable-condition)
  ((spec :initarg  :spec
	 :type     list
	 :reader   filter-construction-error-spec
	 :documentation
	 "The filter specification for which the attempt to construct
a filter instance failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to construct filter based on ~
specification ~S~/rsb::maybe-print-cause/~@:>"
	     (filter-construction-error-spec condition)
	     (rsb:chainable-condition-cause  condition))))
  (:documentation
   "This error is signaled when an attempt to construct a filter
instance based on a filter specification fails."))
