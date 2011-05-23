;;; package.lisp ---
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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

(in-package :cl-user)

(defpackage :rsb
  (:use
   :cl
   :alexandria
   :split-sequence
   :iterate
   :bind
   :hooks) ;; TODO use bind?

  ;; Types
  (:export
   :octet-vector

   :implementation-feedback
   :direction)

  ;; Conditions
  (:export

   :missing-required-initarg

   :rsb-error

   :communication-error

   :informer-creation-failed
   :informer-creation-failed-type

   :participation-failed
   :participation-failed-scope)

  ;; Restarts
  (:export
   :use-scope
   :use-uri)

  ;; Special Variables
  (:export
   :*default-configuration*

   :*subscriber-stream*
   :*publisher-stream*)

  ;; Meta data protocol
  (:export
   :meta-data

   :meta-data-keys
   :meta-data-values

   :meta-data-plist
   :meta-data-alist)

  ;; Scope
  (:export
   :scope
   :scope-components
   :scope-string
   :scope-relative? :scope-absolute?

   :scope=
   :sub-scope?
   :super-scope?

   :make-scope
   :super-scopes
   :merge-scopes

   :intern-scope)

  ;; Event
  (:export
   :event
   :event-id
   :event-scope
   :event-type
   :event-data
   :event-meta-data

   :make-event
   :make-event/typed)

  ;; Participant
  (:export
   :participant
   :participant-id
   :participant-scope)

  ;; Listener
  (:export
   :listener

   :make-listener)

  ;; Receive protocol and `receiver' class
  (:export
   :receive

   :receiver

   :make-receiver)

  ;; Send protocol and `informer' class
  (:export
   :send

   :informer

   :make-informer)

  ;; Macros
  (:export
   ;:with-channel
   :with-listener
   :with-receiver
   :with-enabled-listener :with-disabled-listener
   :with-informer)

  ;; TODO temp utils
  (:export
   :uuid-mixin
   :scope-mixin
   :uri-mixin
   :push-source-mixin
   :hostname)
  (:documentation
   "This package provides basic RSB functionality:
+ subscribing to publishers
+ publishing events
+ event processing"))

(in-package :rsb)


;;; Logging
;;

(log5:defcategory rsb)
