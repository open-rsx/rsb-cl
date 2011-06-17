;;; xml.lisp --- Unit tests for the XML converter.
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

(in-package :rsb.converter.test)

(deftestsuite xml-root (converter-root)
  ()
  (:function
   (doc (string)
     (cxml:parse string (stp:make-builder))))
  (:documentation
   "Unit tests for the XML converter."))

(deftestsuite xml-dom-root (xml-root)
  ()
  (:documentation
   "Unit tests for the xml-dom converter."))

;;; TODO(jmoringe): needs cxml-location support
;; (define-basic-converter-test-cases (:xml-dom)
;;     `((,(doc "") 'string ,(local-time:encode-timestamp 0 0 0 0 1 1 2000))))

(deftestsuite xml-string-root (xml-root)
  ()
  (:documentation
   "Unit tests for the xml-string converter."))

(define-basic-converter-test-cases (:xml-string :domain-test (constantly t))
    `(("<bla/>" :utf-8-xml ,(doc "<bla/>"))
      ("<bla"   :utf-8-xml :error)))

(deftestsuite xml-bytes-root (xml-root)
  ()
  (:documentation
   "Unit tests for the xml-bytes converter."))

(define-basic-converter-test-cases (:xml-bytes :domain-test (constantly t))
    `((,(octetify #(60 98 108 97 47 62)) :bytes-xml ,(doc "<bla/>"))
      (,(octetify #())                   :bytes-xml :error)))
