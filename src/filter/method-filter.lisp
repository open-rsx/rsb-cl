;;; method-filter.lisp --- Event filtering based on method.
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

(defmethod find-filter-class ((spec (eql :method)))
  (find-class 'method-filter))

(defclass method-filter (filter-mixin)
  ((method :initarg  :method
	   :type     (or null string)
	   :reader   filter-method
	   :documentation
	   "Stores the method name to which the filter should restrict
events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :method (missing-required-initarg 'method-filter :method))
  (:documentation
   "This filter discriminates based on the method of events."))

(defmethod matches? ((filter method-filter) (event event))
  (let ((filter-method (filter-method filter))
	(event-method  (event-method event)))
    (case filter-method
      ((nil) (not event-method))
      (t     (and event-method
		  (string= event-method filter-method))))))

(defmethod print-object ((object method-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-method object))))
