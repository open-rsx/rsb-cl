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

(deftestsuite fragmentation-root (transport-spread-root)
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
   (make-notification (sequence-number length id data)
     (make-instance 'rsb.protocol::notification
		    :sequence-number sequence-number
		    :num-data-parts  length
		    :data-part       id
		    :data            data)))
  (:documentation
   "Unit tests for the fragmentation and assembly of
data/notifications."))

(addtest (fragmentation-root
          :documentation
	  "Smoke test for the `merge-fragment' with an `assembly-pool'
instance.")
  assemble-smoke

  (ensure-cases (sequence-number num-parts parts part-ids expected)
      `((0
	 5
	 ("foo" "bar" "baz" "a" "b")
	 (0     1     2     3   4)
	 "foobarbazab")
	(1
	 2
	 ("foo" "baz" "foo" "a" "bar")
	 (0     5     0     2   1)
	 "foobar"))
    ;; We repeat the assembly for all permutation of the fragments.
    (let* ((fragments (iter (for part in parts)
			    (for i    in part-ids)
			    (collect (make-notification
				      sequence-number num-parts i (octetify part))))))
      (map-permutations
       (lambda (permutation)
	 (let* ((pool      (make-instance 'assembly-pool))
		(returns   (map 'list (curry #'merge-fragment pool)
				permutation))
		(assembly  (find-if (complement #'null) returns))
		(result    (assembly-concatenated-data assembly)))
	   (ensure (assembly-complete? assembly))
	   (ensure-same
	    result (octetify expected)
	    :test #'equalp)))
       fragments))))

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
       3)
      ("fooobaar"
       (,(octetify "fooo")
	,(octetify "baar"))
       4))

    (let ((result (fragment-data (octetify data) chunk-size)))
      (ensure-same
       result expected
       :test #'equalp))))

(addtest (fragmentation-root
          :documentation
	  "Do full roundtrips of fragmenting data using
`fragment-data' and then re-assemble the fragments using
`merge-fragments'")
  roundtrip

  (ensure-cases (data chunk-size)
      '(("foobarbazb" 3)
	("fooobaar"   4))

    (let* ((fragments     (fragment-data (octetify data) chunk-size))
	   (notifications (iter (for fragment in    fragments)
				  (for i        :from 0)
				  (collect
				      (make-notification
				       0 (length fragments) i (octetify fragment)))))
	   (pool          (make-instance 'assembly-pool))
	   (result        (assembly-concatenated-data
			   (lastcar (map 'list (curry #'merge-fragment pool)
					 (shuffle notifications))))))
      (ensure-same (octetify data) result
		   :test #'equalp))))

(addtest (fragmentation-root
          :documentation
	  "Ensure that warnings are signaled when invalid fragments
are added to an assembly.")
  warnings

  (let ((sequence-number 0)
	(pool            (make-instance 'assembly-pool)))
    (merge-fragment pool (make-notification
			  sequence-number 3 0 (octetify "foo")))

    (ensure-condition 'invalid-fragment-id
      (merge-fragment pool (make-notification
			    sequence-number 3 5 (octetify "foo"))))

    (ensure-condition 'duplicate-fragment
      (merge-fragment pool (make-notification
			    sequence-number 3 0 (octetify "foo"))))))

(addtest (fragmentation-root
          :documentation
	  "Test `print-object' method on `assembly-pool'.")
  print-smoke

  (let ((pool (make-instance 'assembly-pool)))
    (with-output-to-string (stream)
      (format stream "~A" pool))))

(deftestsuite pruning-assembly-pool-root (fragmentation-root)
  ()
  (:documentation
   "Unit tests for the `pruning-assembly-pool' class."))

(addtest (pruning-assembly-pool-root
          :documentation
	  "Check that old incomplete assemblies actually get pruned.")
  prune

  (let ((pool (make-instance 'pruning-assembly-pool
			     :age-limit 1)))
    (merge-fragment pool (make-notification 0 2 0 (octetify "bla")))
    (let ((count (assembly-pool-count pool)))
      (ensure-same
       count 1
       :test      #'=
       :report    "~@<After submitting a fragment, the count of the pool ~
was ~D, not ~D.~@:>"
       :arguments (count 1)))
    (sleep 2)
    (let ((count (assembly-pool-count pool)))
      (ensure-same
       count 0
       :test #'=
       :report    "~@<After submitting a fragment and waiting for it to ~
get pruned, the count of the pool was ~D, not ~D.~@:>"
       :arguments (count 0)))))

(addtest (pruning-assembly-pool-root
          :documentation
	  "Test `print-object' method on `pruning-assembly-pool'.")
  print-smoke

  (let ((pool (make-instance 'pruning-assembly-pool)))
    (with-output-to-string (stream)
      (format stream "~A" pool))))
