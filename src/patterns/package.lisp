;;; package.lisp --- Package definition for the pattern module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
