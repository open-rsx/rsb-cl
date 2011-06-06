;;; event.lisp --- Unit tests for the event class.
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

(deftestsuite event-root (root)
  ()
  (:documentation
   "Unit tests for the `event' class."))

(addtest (event-root
	  :documentation
	  "Test construction of `event' instances.")
  construction

  ;; Cannot have an `event' without scope
  (ensure-condition 'missing-required-initarg
    (make-instance 'event))

  (let ((event (make-instance 'event
			      :scope (make-scope "/")
			      :data  "foo")))
    (check-event event "/" "foo")
    (ensure (typep (timestamp event :create) 'local-time:timestamp)))

  (let ((event (make-instance 'event
			      :scope             (make-scope "/baz")
			      :data              "bar"
			      :create-timestamp? nil)))
    (check-event event "/baz/" "bar")
    (ensure-null (timestamp event :create))))

(addtest (event-root
          :documentation
	  "Test comparing events for equality using `event='.")
  comparison

  (ensure-cases (scope1 data1 origin1?
		 scope2 data2 origin2?
		 =1? =2? =3? =4?)
      '(("/"    "bar" nil "/" "bar" nil t   nil t   nil)
	("/"    "bar" t   "/" "bar" nil t   nil nil nil)
	("/"    "bar" t   "/" "bar" t   t   nil t   nil)
	("/foo" "bar" nil "/" "bar" nil nil nil nil nil)
	("/"    "baz" nil "/" "bar" nil nil nil nil nil))

    (let* ((origin (uuid:make-v1-uuid))
	   (left   (make-event scope1 data1))
	   (right  (progn
		     (sleep .01)
		     (make-event scope2 data2))))
      (when origin1?
	(setf (event-origin left) origin))
      (when origin2?
	(setf (event-origin right) origin))
      (iter (for (ids? origins? timestamps? expected)
		 in `((nil nil nil ,=1?)
		      (t   nil nil ,=2?)
		      (nil t   nil ,=3?)
		      (nil nil t   ,=4?)))
	    (ensure-same
	     (event= left right
		     :compare-ids?       ids?
		     :compare-origins?   origins?
		     :compare-timestamps timestamps?
		     :data-test          #'equal)
	     expected)))))

(addtest (event-root
          :documentation
	  "Test `print-object' method on `event' class.")
  print

  (check-print (make-event "/foo/bar" "baz"))
  (check-print (make-event "/foo/bar" (make-string 1000
						   :initial-element #\a))))
