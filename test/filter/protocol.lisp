;;; protocol.lisp --- Unit tests for the filter module protocol functions.
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

(in-package :rsb.filter.test)

(deftestsuite protocol-root (filter-root)
  ()
  (:documentation
   "Unit test suite for the protocol functions of the filter module
protocol."))

(addtest (protocol-root
          :documentation
	  "Smoke test for the `filter' function.")
  filter-smoke

  ;; No such filter class.
  (ensure-condition 'filter-construction-error
    (filter :no-such-filter))
  ;; Missing initarg.
  (ensure-condition 'filter-construction-error
    (filter :origin))
  ;; Illegal initarg.
  (ensure-condition 'filter-construction-error
    (filter :origin :origin 5)))
