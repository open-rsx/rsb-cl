;;; package.lisp --- Package definition cl-rsb unit tests.
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

(in-package :cl-user)

(defpackage :rsb.test
  (:use
   :cl
   :alexandria
   :iterate

   :lift

   :rsb)
  (:export
   :root)
  (:documentation
   "This package contains unit tests for the cl-rsb system."))

(in-package :rsb.test)

(deftestsuite root ()
  ()
  (:setup
   (let ((spread-port (asdf:component-property
		       (asdf:find-system :cl-rsb-test) :spread-port)))
     (setf *default-configuration*
	   `(((:transport :spread :host) . "localhost")
	     ((:transport :spread :port) . ,spread-port)))))
  (:documentation
   "Root unit test suite of the cl-rsb system."))

(deftestsuite participant-suite ()
  ()
  (:dynamic-variables
   (*default-configuration* '(((:transport :inprocess :enabled) t))))
  (:function
   (check-participant (participant scope)
     (ensure-same
      (participant-scope participant ) (make-scope scope)
      :test #'scope=)
     (ensure
      (typep (participant-id participant) 'uuid:uuid))))
  (:function
   (check-event (event scope data)
     (ensure-same
      (event-scope event) (make-scope scope)
      :test #'scope=)
     (ensure-same
      (event-data event) data
      :test #'equalp)))
  (:documentation
   "TODO(jmoringe): document"))
