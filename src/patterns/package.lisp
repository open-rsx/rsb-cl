;;; package.lisp --- Package definition for the pattern module.
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

(cl:defpackage :rsb.patterns
  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsb)

  ;; Types
  (:export
   :method-name

   :argument-style)

  ;; Conditions
  (:export
   :no-such-method
   :no-such-method-name

   :remote-call-failed
   :remote-call-failed-method
   :remote-call-failed-request

   :remote-method-execution-error)

  ;; Future protocol
  (:export
   :future-done?
   :future-result
   :future-error)

  ;; `future' class
  (:export
   :future)

  ;; Method protocol
  (:export
   :method-server
   :method-name)

  ;; Remote method protocol
  (:export
   :call)

  ;; Server Protocol
  (:export
   :server
   :server-methods
   :server-method)

  ;; `local-server' class
  (:export
   :local-server
   :make-local-server)

  ;; `remove-server' class
  (:export
   :remote-server
   :make-remote-server)

  ;; Convenience and utility macros
  (:export
   :with-local-server
   :with-methods

   :with-remote-server)

  (:documentation
   "This package contains implementations of communication patterns on
top of the event-driven architecture at the core of RSB.

Currently the following patterns are supported:

+ client-server :: In this communication pattern, a client submits
    requests to a servers which processes the requests and sends
    associated replies to the client."))
