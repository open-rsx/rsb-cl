;;;; fragmentation.lisp --- Unit test for fragmentation/assembly.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

(def-suite fragmentation-root
  :in transport-spread-root
  :description
  "Unit tests for the fragmentation and assembly of
   data/notifications.")
(in-suite fragmentation-root)

(test assemble-smoke
  "Smoke test for the `merge-fragment' with an `assembly-pool'
   instance."

(mapc
 (lambda+ ((sequence-number num-parts parts part-ids expected))
   ;; We repeat the assembly for all permutation of the fragments.
   (let ((fragments (iter (for part in parts)
                          (for i    in part-ids)
                          (collect (a-fragment
                                    sequence-number num-parts i
                                    (octetify part))))))
     (map-permutations
      (lambda (permutation)
        (handler-bind ((fragment-problem #'muffle-warning))
          (let* ((pool      (make-instance 'assembly-pool))
                 (returns   (map 'list (curry #'merge-fragment pool)
                                 permutation))
                 (assembly  (find-if (complement #'null) returns))
                 (result    (assembly-concatenated-data assembly)))
            (is-true (assembly-complete? assembly))
            (is (equalp (octetify expected) result)))))
      fragments)))

 `(;; Everything normal.
   (0
    5
    ("foo" "bar" "baz" "a" "b")
    (0     1     2     3   4)
    "foobarbazab")
   ;; Duplicate and invalid fragments.
   (1
    2
    ("foo" "baz" "foo" "a" "bar")
    (0     5     0     2   1)
    "foobar"))))

(test fragment-smoke
  "Smoke test for the `split-notification' function."

  (mapc
   (lambda+ ((data fragment-size-limit &optional expectec))
     (let+ ((notification (make-notification
                           0 (uuid:make-null-uuid) (rsb:make-scope "/foo")
                           nil :utf-8-string '() `(:create ,(local-time:now))))
            ((&flet do-it ()
               (collect-fragments (split-notification
                                   notification (octetify data)
                                   fragment-size-limit)))))
       (case expected
         (insufficient-room
          (signals insufficient-room (do-it)))
         (t
          (is-true (every (compose (rcurry #'<= fragment-size-limit) ; TODO is-every?
                                   #'pb:packed-size)
                          (do-it)))))))

   `((""                   90)
     ("foobarbazfezwhoop"  85)
     ("foobarbazb"         88)
     ("fooobaar"           89)
     (,(make-string 1000) 100)

     (""                   20 insufficient-room)
     ("bla"                20 insufficient-room))))

(test roundtrip
  "Do full roundtrips of fragmenting data using `split-notification'
   and then re-assemble the fragments using `merge-fragments'."

  (mapc
   (lambda+ ((data fragment-size-limit))
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
       (is (equalp (octetify data) result))))

   `((""                   90)
     ("foobarbazfezwhoop"  85)
     ("foobarbazb"         88)
     ("fooobaar"           89)
     (,(make-string 1000) 100))))

(test warnings
  "Ensure that warnings are signaled when invalid fragments are added
   to an assembly."

  (let ((sequence-number 0)
        (pool            (make-instance 'assembly-pool)))
    ;; Invalid fragment id
    (signals invalid-fragment-id
      (merge-fragment pool (a-fragment
                            sequence-number 3 5 (octetify "foo"))))

    ;; Duplicate fragment
    (merge-fragment pool (a-fragment
                          sequence-number 3 0 (octetify "foo")))
    (signals duplicate-fragment
      (merge-fragment pool (a-fragment
                            sequence-number 3 0 (octetify "foo"))))))

(def-suite assembly-root
  :in fragmentation-root
  :description
  "Unit tests for the `assembly' class.")
(in-suite assembly-root)

(test print-smoke
  "Test `print-object' method on `assembly'."

  (is-false (emptyp
             (princ-to-string
              (make-instance 'assembly
                             :id            (cons 0 (uuid:uuid-to-byte-array
                                                     (uuid:make-null-uuid)))
                             :num-fragments 1)))))

(def-suite assembly-pool-root
  :in fragmentation-root
  :description
  "Unit tests for the `assembly-pool' class.")
(in-suite assembly-pool-root)

(test print-smoke
  "Test `print-object' method on `assembly-pool'."

  (is-false (emptyp (princ-to-string (make-instance 'assembly-pool)))))

(def-suite pruning-assembly-pool-root
  :in fragmentation-root
  :description
  "Unit tests for the `pruning-assembly-pool' class.")
(in-suite pruning-assembly-pool-root)

(test prune
  "Check that old incomplete assemblies actually get pruned."

  (let ((pool (make-instance 'pruning-assembly-pool
                             :age-limit 1)))
    (merge-fragment pool (a-fragment 0 2 0 (octetify "bla")))
    (let ((count (assembly-pool-count pool)))
      (is (= 1 count)
          "~@<After submitting a fragment, the count of the pool was ~
           ~D, not ~D.~@:>"
          count 1))
    (sleep 2)
    (let ((count (assembly-pool-count pool)))
      (is (= 0 count)
          "~@<After submitting a fragment and waiting for it to get ~
           pruned, the count of the pool was ~D, not ~D.~@:>"
          count 0))))

(test print-smoke
  "Test `print-object' method on `pruning-assembly-pool'."

  (is (not (emptyp (princ-to-string
                    (make-instance 'pruning-assembly-pool))))))
