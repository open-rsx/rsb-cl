;;; regex-filter.lisp --- Regular expression filter for event payloads.
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


;;; Protocol
;;

(defgeneric compile-regex (filter regex
			   &key
			   case-sensitive?)
  (:documentation
   "Compile REGEX and store the resulting compiled scanner in
FILTER. CASE-SENSITIVE? controls whether the created scanner performs
case-sensitive matching."))


;;; `regex-filter' class
;;

(defmethod find-filter-class ((spec (eql :regex)))
  (find-class 'regex-filter))

(defclass regex-filter (filter-mixin
			payload-matching-mixin
			fallback-policy-mixin)
  ((regex           :type     string
		    :accessor filter-regex
		    :documentation
		    "Stores regular expression employed by the
filter.")
   (scanner         :reader   filter-scanner
		    :accessor %filter-scanner
		    :documentation
		    "Stores the compiled scanner corresponding to the
regular expression of the filter.")
   (case-sensitive? :initarg  :case-sensitive?
		    :accessor filter-case-sensitive?
		    :initform t
		    :documentation
		    "Stores a boolean which controls whether the
regular expression is matched in case insensitive mode."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :regex (missing-required-initarg 'regex-filter :regex))
  (:documentation
   "Instances of the filter class discriminate events by matching
event payloads that are strings against a given regular expression."))

(defmethod shared-initialize :after ((instance   regex-filter)
                                     (slot-names t)
                                     &key
				     regex)
  (check-type regex string "a string")

  (setf (filter-regex instance) regex))

(defmethod (setf filter-regex) :before ((new-value string)
					(filter    regex-filter))
  "Create a new scanner for FILTER for the regex NEW-VALUE."
  (compile-regex filter new-value
		 :case-sensitive? (filter-case-sensitive? filter)))

(defmethod (setf filter-case-sensitive?) :before ((new-value t)
						  (filter    regex-filter))
  "Create a new scanner for FILTER for the regex NEW-VALUE."
  (compile-regex filter (filter-regex filter)
		 :case-sensitive? new-value))

(defmethod compile-regex ((filter regex-filter) (regex string)
			  &key
			  case-sensitive?)
  (setf (%filter-scanner filter)
	(ppcre:create-scanner
	 regex :case-insensitive-mode (not case-sensitive?))))

(defmethod payload-matches? ((filter regex-filter) (payload string))
  (bind (((:accessors-r/o (scanner filter-scanner)) filter))
    (ppcre:scan scanner payload)))

(defmethod print-object ((object regex-filter) stream)
  (bind (((:accessors-r/o (regex           filter-regex)
			  (case-sensitive? filter-case-sensitive?)
			  (fallback-policy filter-fallback-policy)) object))
   (print-unreadable-object (object stream :type t :identity t)
     (format stream "~S~:[/i~;~] or ~A"
	     regex case-sensitive? fallback-policy))))
