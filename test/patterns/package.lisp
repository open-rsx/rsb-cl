;;; package.lisp --- Package definition for unit tests of the patterns module.
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

(cl:defpackage :rsb.patterns.test
  (:use
   :cl
   :bind
   :iterate
   :lift

   :rsb
   :rsb.patterns

   :rsb.test)

  (:import-from :rsb.patterns
   :method1
   :local-method)

  (:documentation
   "This package contains unit tests for the patterns module of
cl-rsb."))

(cl:in-package :rsb.patterns.test)

(deftestsuite patterns-root (root)
  ()
  (:documentation
   "Root unit test suite for the patterns module of cl-rsb."))
