;;; threaded-receiver-mixin.lisp ---
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

(in-package :rsb.transport.test)

(defclass mock-receiver (threaded-receiver-mixin)
  ())

(defmethod receive-messages ((receiver mock-receiver))
  (iter (while t) (sleep 100)))

(deftestsuite threaded-receiver-mixin-root (transport-root)
  ()
  (:documentation
   "Unit tests for the `threaded-receiver-mixin' class."))

(addtest (threaded-receiver-mixin-root
          :documentation
	  "Smoke test for the `threaded-receiver-mixin' class.")
  smoke

  ;;; TODO(jmoringe): Currently not usable because spread (even when
  ;;; just loaded) likes to block/handle some signals
  (let ((receiver (make-instance 'mock-receiver)))
    ;; (notify receiver (make-scope "/") :attached)
    ;; (sleep 1)
    ;; (notify receiver (make-scope "/") :detached)
    ))
