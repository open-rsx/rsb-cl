;;;; fragmentation.lisp --- Unit test for fragmentation/assembly.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

(deftestsuite fragmentation-root (transport-spread-root)
  ()
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
                           (collect (a-fragment
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
          "Smoke test for the `split-notification' function.")
  fragment-smoke

  (ensure-cases (data fragment-size-limit &optional expected)
      `((""                   90)
        ("foobarbazfezwhoop"  85)
        ("foobarbazb"         88)
        ("fooobaar"           89)
        (,(make-string 1000) 100)

        (""                   20 insufficient-room)
        ("bla"                20 insufficient-room))

    (let+ ((notification (make-notification
                          0 (uuid:make-null-uuid) (rsb:make-scope "/foo")
                          nil :utf-8-string '() `(:create ,(local-time:now))))
           ((&flet do-it ()
              (collect-fragments (split-notification
                                  notification (octetify data)
                                  fragment-size-limit)))))
      (case expected
        (insufficient-room
         (ensure-condition 'insufficient-room (do-it)))
        (t
         (ensure (every (compose (rcurry #'<= fragment-size-limit)
                                 #'pb:packed-size)
                        (do-it))))))))

(addtest (fragmentation-root
          :documentation
          "Do full roundtrips of fragmenting data using
           `split-notification' and then re-assemble the fragments
           using `merge-fragments'.")
  roundtrip

  (ensure-cases (data fragment-size-limit)
      `((""                   90)
        ("foobarbazfezwhoop"  85)
        ("foobarbazb"         88)
        ("fooobaar"           89)
        (,(make-string 1000) 100))

    (let* ((notification  (make-notification
                           0 (uuid:make-null-uuid) (rsb:make-scope "/foo")
                           nil :utf-8-string '() `(:create ,(local-time:now))))
           (notifications (collect-fragments
                           (split-notification
                            notification (octetify data)
                            fragment-size-limit)))
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
    (merge-fragment pool (a-fragment
                          sequence-number 3 0 (octetify "foo")))

    (ensure-condition 'invalid-fragment-id
      (merge-fragment pool (a-fragment
                            sequence-number 3 5 (octetify "foo"))))

    (ensure-condition 'duplicate-fragment
      (merge-fragment pool (a-fragment
                            sequence-number 3 0 (octetify "foo"))))))

(deftestsuite assembly-root (fragmentation-root)
  ()
  (:documentation
   "Unit tests for the `assembly' class."))

(addtest (assembly-root
          :documentation
          "Test `print-object' method on `assembly'.")
  print-smoke

  (ensure (not (emptyp
                (princ-to-string
                 (make-instance 'assembly
                                :id            (cons 0 (uuid:uuid-to-byte-array
                                                        (uuid:make-null-uuid)))
                                :num-fragments 1))))))

(deftestsuite assembly-pool-root (fragmentation-root)
  ()
  (:documentation
   "Unit tests for the `assembly-pool' class."))

(addtest (assembly-pool-root
          :documentation
          "Test `print-object' method on `assembly-pool'.")
  print-smoke

  (ensure (not (emptyp (princ-to-string
                        (make-instance 'assembly-pool))))))

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
    (merge-fragment pool (a-fragment 0 2 0 (octetify "bla")))
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

  (ensure (not (emptyp (princ-to-string
                        (make-instance 'pruning-assembly-pool))))))
