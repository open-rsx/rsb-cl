;;; package.lisp --- Package definition for unit tests of the transport.spread module.
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

(cl:in-package :cl-user)

(defpackage :rsb.transport.spread.test
  (:use
   :cl
   :alexandria
   :iterate
   :lift

   :rsb
   :rsb.transport
   :rsb.transport.spread

   :rsb.test
   :rsb.transport.test)

  (:import-from :rsb.transport.spread
   :assembly-complete?
   :assembly-concatenated-data

   :assembly-pool-count
   :merge-fragment

   :assembly-pool
   :pruning-assembly-pool

   :fragment-data

   :*scope->groups-cache*
   :*scope->groups-cache-max-size*
   :scope->group
   :scope->groups/no-cache
   :scope->groups

   :connection

   :in-connector
   :message->event

   :in-pull-connector

   :out-connector)

  (:documentation
   "This package contains unit tests for the transport.spread
module"))

(in-package :rsb.transport.spread.test)

(deftestsuite transport-spread-root (transport-root)
  ()
  (:documentation
   "Root unit test suite for the transport.spread module."))
