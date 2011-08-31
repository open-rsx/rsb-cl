;;; connector-class.lisp --- Unit tests for the connector-class class.
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

(deftestsuite connector-class-root (transport-root)
  ()
  (:documentation
   "Test suite for the `connector-class' class."))

(addtest (connector-class-root
          :documentation
	  "Test constructing an instance of `connector-class'.")
  construction/valid

  (eval
   '(defclass foo ()
      ((an-option :initarg  :an-option
		  :type     boolean
		  :initform t
		  :documentation
		  "doc"))
      (:metaclass connector-class)
      (:direction :in-push)
      (:wire-type string)
      (:schemas   :whoop)
      (:options
       (:an-option &slot))))

  (let ((class (find-class 'foo)))
    (ensure-same (connector-direction class) :in-push)
    (ensure-same (connector-wire-type class) 'string)
    (ensure-same (connector-schemas class)   '(:whoop))
    (ensure-same (connector-options class)   '((:an-option boolean
						:default     t
						:description "doc")))))

(addtest (connector-class-root
          :documentation
	  "Test error behavior for an invalid constructing an instance
of `connector-class'.")
  construction/invalid

  ;; We request the option no-such-option to be constructed from the
  ;; slot of the same name. Since there is no such slot, an error has
  ;; to be signaled.
  (ensure-condition 'error
    (eval '(defclass bar ()
	    ()
	    (:metaclass connector-class)
	    (:options
	     (:no-such-option &slot))))))
