;;; fragmentation.lisp --- Unit test for fragmentation/assembly.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.transport.spread.test)

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
   (make-fragment (sequence-number length id data)
     (let* ((event-id     (make-instance
			   'rsb.protocol:event-id
			   :sender-id       (uuid:uuid-to-byte-array
					     (uuid:make-null-uuid))
			   :sequence-number sequence-number))
	    (notification (make-instance 'rsb.protocol:notification
					 :event-id event-id
					 :data     data)))
       (make-instance 'rsb.protocol:fragmented-notification
		      :notification   notification
		      :num-data-parts length
		      :data-part      id))))
  (:function
   (make-event* (data)
     (let ((event (make-event "/foo" (octetify data))))
       (setf (event-origin event)
	     (uuid:make-null-uuid)
	     (event-sequence-number event)
	     0)
       event)))
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
    (let ((fragments (iter (for part in parts)
			   (for i    in part-ids)
			   (collect (make-fragment
				     sequence-number num-parts i (octetify part))))))
      (map-permutations
       (lambda (permutation)
	 (let* ((pool      (make-instance 'assembly-pool))
		(returns   (map 'list (curry #'merge-fragment pool)
				permutation))
		(assembly  (find-if (complement #'null) returns))
		(result    (assembly-concatenated-data assembly)))
	   (ensure      (assembly-complete? assembly))
	   (ensure-same result (octetify expected)
			:test #'equalp)))
       fragments))))

(addtest (fragmentation-root
          :documentation
	  "Smoke test for the `event->notifications' function.")
  fragment-smoke

  (ensure-cases (data chunk-size error?)
      `((""                   90 nil)
	("foobarbazfezwhoop"  85 nil)
	("foobarbazb"         88 nil)
	("fooobaar"           89 nil)
	(,(make-string 1000) 100 nil)

	(""                   20 t)
	("bla"                20 t))

    (let ((event (make-event* data)))
      (if error?
	  (ensure-condition 'insufficient-room
	    (event->notifications :fundamental-null event chunk-size))
	  (let ((result (event->notifications
			 :fundamental-null event chunk-size)))
	    (ensure (every (compose (rcurry #'<= chunk-size)
				    #'pb:packed-size)
			   result)))))))

(addtest (fragmentation-root
          :documentation
	  "Do full roundtrips of fragmenting data using
`event->notifications' and then re-assemble the fragments using
`merge-fragments'.")
  roundtrip

  (ensure-cases (data chunk-size)
      `((""                   90)
	("foobarbazfezwhoop"  85)
	("foobarbazb"         88)
	("fooobaar"           89)
	(,(make-string 1000) 100))

    (let* ((event         (make-event* (octetify data)))
	   (notifications (event->notifications :fundamental-null event chunk-size))
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
    (merge-fragment pool (make-fragment
			  sequence-number 3 0 (octetify "foo")))

    (ensure-condition 'invalid-fragment-id
      (merge-fragment pool (make-fragment
			    sequence-number 3 5 (octetify "foo"))))

    (ensure-condition 'duplicate-fragment
      (merge-fragment pool (make-fragment
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
    (merge-fragment pool (make-fragment 0 2 0 (octetify "bla")))
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
