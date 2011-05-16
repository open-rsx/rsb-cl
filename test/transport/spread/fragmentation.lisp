;;; fragmentation.lisp --- Unit test for fragmentation/assembly.
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

(in-package :rsb.transport.spread.test)

(deftestsuite fragmentation-root (spread-root)
  ()
  (:function
   (octetify (data)
     (etypecase data
       (string
	(sb-ext:string-to-octets data))
       (sequence
	(coerce data 'octet-vector))
       (t
	data))))
  (:function
   (make-notification (uuid length id data)
     (make-instance 'rsb.protocol::notification
		    :id             (princ-to-string uuid)
		    :num-data-parts length
		    :data-part      id
		    :data           (make-instance 'rsb.protocol::attachment
						   :binary data))))
  (:documentation
   "Unit tests for the fragmentation and assembly of
data/notifications."))

(addtest (fragmentation-root
          :documentation
	  "TODO(jmoringe): document")
  assemble-smoke

  (let* ((uuid      (uuid:make-v1-uuid))
	 (parts     '("foo" "bar" "baz" "a" "b"))
	 (fragments (iter (for part in parts)
			  (for i :from 0)
			  (collect (make-notification
				    uuid 5 i (octetify part)))))
	 (pool      (make-instance 'assembly-pool))
	 (returns   (map 'list (curry #'merge-fragment pool)
			 (shuffle fragments)))
	 (assembly  (lastcar returns))
	 (result    (assembly-concatenated-data assembly)))
    (ensure (assembly-complete? assembly))
    (ensure-same
     result
     (octetify
      (apply #'concatenate 'string parts))
     :test #'equalp)))

(addtest (fragmentation-root
          :documentation
	  "Smoke test for the `fragment-data' function.")
  fragment-smoke

  (ensure-cases (data expected chunk-size)
    `(("foobarbazb"
       (,(octetify '(102 111 111))
	,(octetify '(98 97 114))
	,(octetify '(98 97 122))
	,(octetify '(98)))
       3))

    (let ((result (fragment-data (octetify data) chunk-size)))
      (ensure-same
       result expected
       :test #'equalp))))

(addtest (fragmentation-root
          :documentation
	  "TODO(jmoringe): document")
  roundtrip

  (ensure-cases (data chunk-size)
      '(("foobarbazb" 3))

    (let* ((fragments     (fragment-data (octetify data) chunk-size))
	   (uuid          (uuid:make-v1-uuid))
	   (notifications (iter (for fragment in    fragments)
				(for i        :from 0)
				(collect
				    (make-notification
				     uuid (length fragments) i (octetify fragment)))))
	   (pool          (make-instance 'assembly-pool))
	   (result        (assembly-concatenated-data
			   (lastcar (map 'list (curry #'merge-fragment pool)
					 (shuffle notifications))))))
      (ensure-same (octetify data) result
		   :test #'equalp))))
