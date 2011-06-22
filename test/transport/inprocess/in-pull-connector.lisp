;;; in-pull-connector.lisp --- Unit tests for the inprocess:in-pull-connector class.
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

(in-package :rsb.transport.inprocess.test)

(deftestsuite in-pull-connector-root (transport-inprocess-root
				      connector-suite)
  ()
  (:documentation
   "Root unit tests suite for the `in-pull-connector' class."))

(define-basic-connector-test-cases in-pull-connector
    :expected-direction :in-pull
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
