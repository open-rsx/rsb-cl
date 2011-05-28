;;; xpath-filter.lisp ---
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

(defclass xpath-filter (filter-mixin)
  ((xpath          :initarg  :xpath
		   :type     string
		   :accessor filter-xpath
		   :initform "."
		   :documentation
		   "The XPath used by the filter to discriminate
events.")
   (compiled-xpath :initarg  :compiled-xpath
		   :type     (or null function)
		   :reader   filter-compiled-xpath
		   :initform nil
		   :documentation
		   "A compiled version of the XPath of the
filter. Computed lazily."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This filter discriminates based on XML content contained in
events."))

(defmethod matches? :before ((filter xpath-filter) (event event))
  "Compile the XPath, if it has not yet been compiled."
  (with-slots (xpath compiled-xpath) filter
    (unless compiled-xpath
      (setf compiled-xpath (xpath:compile-xpath xpath)))))

(defmethod matches? ((filter xpath-filter) (event event))
  "Match the data of EVENT against the XPath of FILTER."
  (bind (((:slots compiled-xpath) filter)
	 (data (event-data event))
	 (doc  (if (stringp data)
		   (cxml:parse data (stp:make-builder))
		   data)))
    (not (xpath:node-set-empty-p
	  (xpath:evaluate-compiled compiled-xpath doc t)))))

(defmethod print-object ((object xpath-filter) stream)
  (with-slots (xpath compiled-xpath) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A~:[~; [compiled]~]"
	      xpath compiled-xpath))))
