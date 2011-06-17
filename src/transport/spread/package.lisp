;;; package.lisp --- Package definition for transport.spread module.
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

(defpackage :rsb.transport.spread
  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :iterate
   :bind

   :rsb
   :rsb.event-processing
   :rsb.transport)

  ;; Conditions
  (:export

   :decoding-error
   :decoding-error-encoded

   :encoding-error
   :encoding-error-event

   :assembly-problem
   :assembly-problem-assembly

   :fragment-problem
   :assembly-problem-fragment

   :invalid-fragment-id

   :duplicate-fragment)

  ;; Exported for unit tests
  (:export
   :assembly-complete?
   :assembly-concatenated-data

   :assembly-pool-count
   :merge-fragment

   :assembly-pool
   :pruning-assembly-pool

   :fragment-data

   :*scope-group-cache*
   :*scope-group-cache-max-size*
   :scope->group
   :scope->group/no-cache

   :connection

   :in-connector
   :message->event

   :in-pull-connector

   :out-connector)

  (:documentation
   "This package contains a transport implementation based on the
spread group communication system."))

(in-package :rsb.transport.spread)

(log5:defcategory rsb.transport.spread)

(defmacro log1 (category format &rest args)
  `(log5:log-for (or :rsb.transport.spread ,(intern (string category) :log5))
		 ,format ,@args))
