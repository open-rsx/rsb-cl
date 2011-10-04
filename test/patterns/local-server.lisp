;;; local-server.lisp --- Unit tests for the local-server class.
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

(deftestsuite local-method-root (patterns-root)
  ()
  (:documentation
   "Test suite for `local-method' class."))

(addtest (local-method-root
          :documentation
	  "Test constructing `local-method' instances.")
  construction

  ;; Missing :callback initarg
  (ensure-condition 'missing-required-initarg
    (make-instance 'local-method :name "foo")))

(deftestsuite local-server-root (patterns-root)
  ((simple-server (make-instance 'local-server
				 :scope             "/localserver"
				 :transport-options '((:inprocess)))))
  (:documentation
   "Test suite for the `local-server' class."))

(addtest (local-server-root
          :documentation
	  "Test adding methods to a `local-server' instance.")
  set-method

  (ensure-cases (name method args expected)
      `(("foo"          ,#'(lambda ()) ()                   t)
	("foo"          ,#'(lambda ()) ()                   t)
	("foo"          nil            ()                   nil)

	("bar"          ,#'(lambda ()) (:argument :event)   t)
	("bar"          ,#'(lambda ()) (:argument :payload) t)

	;; invalid method name => error
	("%invalidname" ,#'(lambda ()) ()                   :error)
	;; invalid argument style => error
	("bar"          ,#'(lambda ()) (:argument :foo)     :error))

    (if (eq expected :error)
	(ensure-condition 'type-error
	  (setf (apply #'server-method simple-server name args) method))
	(let ((result-1 (setf (apply #'server-method simple-server name args) method))
	      (result-2 (server-method simple-server name
				       :error? nil)))
	  (if (eq expected t)
	      (progn
		(ensure result-1)
		(ensure result-2))
	      (progn
		(ensure-same result-1 expected)
		(ensure-same result-2 expected)))))))
