;;; xpath-filter.lisp --- XPath-based filtering.
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

(cl:in-package :rsb.filter)


;;; Protocol
;;

(defgeneric compile-xpath (filter xpath)
  (:documentation
   "Produce and return a compiled representation of XPATH that can be
used with FILTER."))


;;; `xpath-filter' class
;;

(defmethod find-filter-class ((spec (eql :xpath)))
  (find-class 'xpath-filter))

(defclass xpath-filter (filter-mixin
			payload-matching-mixin
			fallback-policy-mixin)
  ((xpath          :type     string
		   :accessor filter-xpath
		   :documentation
		   "The XPath used by the filter to discriminate
events.")
   (compiled-xpath :type     function
		   :reader   filter-compiled-xpath
		   :writer   (setf %filter-compiled-xpath)
		   :documentation
		   "A compiled version of the XPath of the filter."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :xpath (missing-required-initarg 'xpath-filter :xpath))
  (:documentation
   "This filter discriminates events based on XPath expressions. It is
applicable to payloads for which an implementation of the XPath
interface is available. Examples include strings (via XML parsing),
XML DOM objects and protocol buffer messages."))

(defmethod shared-initialize :after ((instance   xpath-filter)
                                     (slot-names t)
                                     &key
				     xpath)
  (check-type xpath xpath::xpath-expr
	      "an XPath string or an XPath sexp expression")

  (setf (filter-xpath instance) xpath))

(defmethod (setf filter-xpath) :before ((new-value string)
					(filter    xpath-filter))
  "Compile the XPath."
  (setf (%filter-compiled-xpath filter)
	(compile-xpath filter new-value)))

(defmethod compile-xpath ((filter xpath-filter)
			  (xpath  string))
  (xpath:compile-xpath xpath))

(defmethod print-object ((object xpath-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (filter-xpath object))))


;;; Utility functions
;;

(defun xpath-result->filter-result (result)
  "Return a non-nil if RESULT represents a matching XPath result and
nil otherwise."
  (typecase result
    (xpath:node-set (not (xpath:node-set-empty-p result)))
    (string         (emptyp result))
    (t              result)))
