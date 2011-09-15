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
	  "Test computation of event sequence numbers.")
  id-computation

  ;; Test some examples. Note that event ids cannot be computed
  ;; without origin id.
  (ensure-cases (sequence-number origin expected)
      `((0 nil nil)
	(0
	 ,(make-id "D8FBFEF4-4EB0-4C89-9716-C425DED3C527")
	 ,(make-id "84F43861-433F-5253-AFBB-A613A5E04D71"))
	(378
	 ,(make-id "BF948D47-618F-4B04-AAC5-0AB5A1A79267")
	 ,(make-id "BD27BE7D-87DE-5336-BECA-44FC60DE46A0")))
    (let ((event (apply #'make-instance 'event
			:sequence-number sequence-number
			:scope           (make-scope "/")
			(when origin
			  (list :origin origin)))))
      (if expected
	  (ensure-same (event-id event) expected
		       :test #'rsb::uuid=)
	  (ensure-null (event-id event))))))

(addtest (event-root
          :documentation
	  "Test comparing events for equality using `event='.")
  comparison

  (ensure-cases (scope1 data1 origin1?
		 scope2 data2 origin2?
		 =1? =2? =3? =4?)
      '(("/"    "bar" nil     "/" "bar" nil     t   nil t   nil)
	("/"    "bar" t       "/" "bar" nil     t   nil nil nil)
	("/"    "bar" t       "/" "bar" t       t   nil t   nil)
	("/foo" "bar" nil     "/" "bar" nil     nil nil nil nil)
	("/"    "baz" nil     "/" "bar" nil     nil nil nil nil))

    (let* ((origin (uuid:make-v1-uuid))
	   (left   (make-event scope1 data1))
	   (right  (progn
		     (sleep .000001) ;; force different create times
		     (make-event scope2 data2)) ))

      (setf (event-sequence-number left) 0)
      (when origin1?
	(setf (event-origin left) origin))

      (setf (event-sequence-number right) 1)
      (when origin2?
	(setf (event-origin right) origin))

      (iter (for (sequence-numbers? origins? timestamps? expected)
		 in `((nil nil nil ,=1?)
		      (t   nil nil ,=2?)
		      (nil t   nil ,=3?)
		      (nil nil t   ,=4?)))
	    (ensure-same
	     (event= left right
		     :compare-sequence-numbers? sequence-numbers?
		     :compare-origins?          origins?
		     :compare-timestamps        timestamps?
		     :data-test                 #'equal)
	     expected
	     :report "~@<When compared ~:[without~;with~] sequence ~
numbers, ~:[without~;with~] origins and ~:[without~;with~] timestamps, ~
events ~A and ~A were ~:[not ~;~]equal, but expected ~:[not ~;~]~
to be.~@:>"
	     :arguments (sequence-numbers? origins? timestamps?
			 left right (not expected) expected))))))

(addtest (event-root
          :documentation
	  "Test `print-object' method on `event' class.")
  print

  (check-print (make-event "/foo/bar" "baz"))
  (check-print (make-event "/foo/bar" (make-string 1000
						   :initial-element #\a))))
