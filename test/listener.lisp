;;; listener.lisp --- Unit tests for listener.
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

(in-package :rsb.test)

(deftestsuite listener-root (root
			     participant-suite)
  ()
  (:documentation
   "Unit tests for the `listener' class and `make-listener'
function."))

(addtest (listener-root
          :documentation
	  "Test participating in a channel via `make-listener'.")
  construction

  (ensure-cases (arg expected-scope)
      '(("/listener/construction"                      "/listener/construction")
	("inprocess://localhost/listener/construction" "/listener/construction"))

    (let ((listener (make-listener arg)))
      (check-participant listener expected-scope))))

(addtest (listener-root
          :documentation
	  "Test receiving data sent by an informer.")
  receive

  ;; Test blocking receive
  (with-informer (informer "/foo" t)
    (with-listener (listener "/foo")
      (iter (repeat 100)
	    (send informer "<foo/>")
	    (send informer "<bar/>")))))

;; (addtest (listener-root
;;           :documentation
;;	  "Test binding *listener-stream*.")
;;   listener-stream
;;
;;   (with-informer (informer "sub-stream" t)
;;     (with-listener (sub "sub-stream")
;;       (send informer "<foo/>")
;;       (ensure-same
;;        (concatenate 'string +optional-xml-declaration+ "<foo>.*</foo>$")
;;        (with-output-to-string (stream)
;;	 (let ((*listener-stream* stream))
;;	   (receive sub :block? t)))
;;        :test #'regexp-matches))))
