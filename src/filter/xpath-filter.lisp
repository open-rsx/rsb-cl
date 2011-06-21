;;; xpath-filter.lisp --- XPath filtering based on XML event payload.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.filter)

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
		   :documentation
		   "A compiled version of the XPath of the filter."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :xpath (missing-required-initarg 'xpath-filter :xpath))
  (:documentation
   "This filter discriminates based on XML content contained in
events."))

(defmethod shared-initialize :after ((instance   xpath-filter)
                                     (slot-names t)
                                     &key
				     xpath)
  (check-type xpath string "a string")

  (setf (filter-xpath instance) xpath))

(defmethod (setf filter-xpath) :before ((new-value string)
					(filter    xpath-filter))
  "Compile the XPath."
  (setf (slot-value filter 'compiled-xpath)
	(xpath:compile-xpath new-value)))

;; TODO ill-formed documents: return nil or error?
(defmethod payload-matches? ((filter xpath-filter) (payload stp:document))
  "Match the data of EVENT against the XPath of FILTER."
  (bind (((:accessors-r/o
	   (compiled-xpath filter-compiled-xpath)) filter))
    (xpath-result->filter-result
     (xpath:evaluate-compiled compiled-xpath payload t))))

(defmethod payload-matches? ((filter xpath-filter) (payload string))
  "Try to parse PAYLOAD as an XML document and proceed with the
matching in case of success."
  (let ((doc (ignore-errors
	       (cxml:parse payload (stp:make-builder)))))
    (if doc
	(payload-matches? filter doc)
	:cannot-tell)))

(defmethod print-object ((object xpath-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (filter-xpath object))))


;;; Utility functions
;;

(defun xpath-result->filter-result (result)
  "Return a non-nil if RESULT represents a matching XPath result and
nil otherwise."
  (etypecase result
    (xpath:node-set (not (xpath:node-set-empty-p result)))
    (string         (emptyp result))
    (t              result)))
