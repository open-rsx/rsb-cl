;;; server.lisp --- Unit tests for the method1 and server classes.
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

(in-package :rsb.patterns.test)

(deftestsuite method1-root (patterns-root)
  ()
  (:documentation
   "Test suite for `method1' class."))

(addtest (method1-root
          :documentation
	  "Test constructing `method1' instances.")
  construction

  ;; Missing :name initarg
  (ensure-condition 'missing-required-initarg
    (make-instance 'method1))

  ;; Some illegal method names
  (ensure-cases (name)
      '("illegal-name" "illegal/name" "illegal name" "i113g4l n4m3")
    (ensure-condition 'type-error
      (make-instance 'method1 :name name))))

(deftestsuite server-root (patterns-root)
  ((simple-server (make-instance 'server :scope "/server"))
   (simple-method (make-instance 'method1 :name "foo")))
  (:documentation
   "Test suite for the `server' class."))

(addtest (server-root
          :documentation
	  "Test adding methods to a `server' instance.")
  set-method

  (ensure-cases (name method expected)
      `(("foo"          ,simple-method ,simple-method)
	("foo"          ,simple-method ,simple-method)
	("foo"          nil            nil)

	;; invalid method name => error
	("%invalidname" ,simple-method :error))

    (if (eq expected :error)
	(ensure-condition 'type-error
	  (setf (server-method simple-server name) method))

	(let ((result-1 (setf (server-method simple-server name) method))
	      (result-2 (server-method simple-server name
				       :error? nil)))
	  (ensure-same result-1 expected)
	  (ensure-same result-2 expected)))))
