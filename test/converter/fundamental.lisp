;;; fundamental.lisp --- Unit tests for "fundamental" converters.
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

(deftestsuite fundamental-root (converter-root)
  ()
  (:documentation
   "Root unit test suite for fundamental converters."))


;;; Converter fundamental-string
;;

(deftestsuite fundamental-string-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-string' converter."))

(define-basic-converter-test-cases :fundamental-string
    `((,(octetify #())         :string "")
      (,(octetify #(65 65 65)) :string "AAA")))
