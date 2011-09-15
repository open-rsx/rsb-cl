;;; server.lisp --- Unit tests for the method1 and server classes.
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

(in-package :rsb.patterns.test)

(deftestsuite method1-root (patterns-root)
  ()
  (:documentation
   "Test suite for `method1' class."))

(addtest (method1-root
          :documentation
	  "Test constructing `method1' instances.")
  construction

  ;; Missing :name initarg
  (ensure-condition 'missing-required-initarg
    (make-instance 'method1))

  ;; Some illegal method names
  (ensure-cases (name)
      '("illegal-name" "illegal/name" "illegal name" "i113g4l n4m3")
    (ensure-condition 'type-error
      (make-instance 'method1 :name name))))
