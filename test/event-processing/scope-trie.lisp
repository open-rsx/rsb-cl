;;;; scope-trie.lisp --- Tests for the scope-trie and sink-scope-trie classes.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

;;; Utilities.

(defparameter *scope-trie-test-a-few-scopes*
  (mapcar #'make-scope '("/a/b/c" "/a/b/c" "/a/e/c" "/a/b" "/a" "/")))

(defparameter *scope-trie-test-lots-of-scopes*
  (let ((result '()))
    (map-permutations (lambda (components)
                        (push (make-scope components) result))
                      '("a" "b" "c" "d" "e"))
    result))

(defun make-filled-scope-trie (&optional
                               (scopes *scope-trie-test-a-few-scopes*))
  (let+ ((trie (make-scope-trie))
         ((&flet add-scope (scope)
            (scope-trie-update
             (lambda (value valuep)
               (values (list* (scope-string scope) (when valuep value)) t))
             scope trie))))
    ;; Add some scopes.
    (mapc #'add-scope scopes)
    trie))

;;; `scope-trie'

(deftestsuite event-processing-scope-trie-root (event-processing-root)
  ()
  (:documentation
   "Test suite for the `scope-trie' data structure."))

(addtest (event-processing-scope-trie-root
          :documentation
          "Smoke test for the `scope-trie' data structure.")
  smoke

  ;; Order of additions and removals must not matter.
  (map-permutations
   (lambda (scopes)
     (let+ ((trie (make-scope-trie))
            ((&flet add-scope (scope)
               (scope-trie-update
                (lambda (value valuep)
                  (values (list* (scope-string scope) (when valuep value)) t))
                scope trie)))
            ((&flet check-empty ()
               (dolist (scope scopes)
                 (ensure-same (nth-value 1 (scope-trie-get scope trie)) nil)))))
       ;; We repeat the process to ensure that deleting all values
       ;; restores the initial state and that the restored initial
       ;; state permits add values again. There was a bug regarding
       ;; removing the value associated to the root scope at some
       ;; point.
       (loop :repeat 2 :do
          ;; Initially empty/empty again.
          (check-empty)
          ;; Add some entries.
          (mapc #'add-scope scopes)
          ;; Check stored entries.
          (dolist (scope scopes)
            (let* ((count    (count scope *scope-trie-test-a-few-scopes*
                                    :test #'scope=))
                   (expected (make-list count
                                        :initial-element (scope-string scope))))
              (ensure-same (scope-trie-get scope trie) (values expected t)
                           :test #'equal)))
          ;; Remove all entries.
          (mapc (rcurry #'scope-trie-rem trie) scopes))))
   *scope-trie-test-a-few-scopes*))

(addtest (event-processing-scope-trie-root
          :documentation
          "Smoke test for the `scope-trie-map' function.")
  scope-trie-map/smoke

  (let ((trie (make-filled-scope-trie)))
    (ensure-cases (scope expected)
        '(("/"      (("/")))
          ("/b"     (("/")))
          ("/a"     (("/") ("/a/")))
          ("/a/b"   (("/") ("/a/") ("/a/b/")))
          ("/a/b/c" (("/") ("/a/") ("/a/b/") ("/a/b/c/" "/a/b/c/"))))
      (let ((scope  (make-scope scope))
            (values '()))
        (scope-trie-map (lambda (value) (push value values)) scope trie)
        (ensure-same (reverse values) expected :test #'equal)))))

(defun cleanup-test (thunk)
  (let* ((count        100)
         (random-scope (random-scope :length (random-integer 0 4)))
         (queue        (map-into (make-list count) random-scope))
         (trie         (make-scope-trie))
         (thunk        (ensure-function thunk)))
    (loop :for i :below 200000
       :for remove = (pop queue)
       :for add = (funcall random-scope) :do
       (appendf queue (list add))
       (ensure-same (length queue) count)
       (setf (scope-trie-get add trie) i)
       (funcall thunk trie remove))))

(addtest (event-processing-scope-trie-root
          :documentation
          "Make sure that removing entries from the `scope-trie' via
           `scope-trie-rem' cleans everything up.")
  scope-trie-rem/cleanup

  (cleanup-test (lambda (trie remove) (scope-trie-rem remove trie))))

(addtest (event-processing-scope-trie-root
          :documentation
          "Make sure that removing entries from the `scope-trie' via
           `scope-trie-update' cleans everything up.")
  scope-trie-update/cleanup

  (cleanup-test (lambda (trie remove)
                  (scope-trie-update
                   (lambda (value value?)
                     (declare (ignore value value?))
                     (values nil nil))
                   remove trie))))

(defun %scope-trie-test-make-adder (trie scopes i)
  (named-lambda adder ()
    (sleep (+ 0.001 (random 0.0001)))
    (loop :for scope :in scopes :do
       (scope-trie-update
        (lambda (value valuep)
          (values (list* i (when valuep value)) t))
        scope trie))))

(defun %scope-trie-test-make-remover (trie scopes i)
  (named-lambda remover ()
    (loop :for scope :in scopes :do
       (scope-trie-update (lambda (value value?)
                            (declare (ignore value?))
                            (let ((new (remove i value)))
                              (values new new)))
                          scope trie))))

(addtest (event-processing-scope-trie-root
          :documentation
          "Multi-threaded stress test for the `scope-trie' data
           structure exercising concurrent insertions.")
  stress.1

  (loop :repeat 1000 :do
     (let ((scopes       *scope-trie-test-lots-of-scopes*)
           (thread-count 3)
           (trie         (make-scope-trie)))
       ;; Run concurrent insertions.
       (mapc #'bt:join-thread
             (mapcar (compose #'bt:make-thread
                              (curry #'%scope-trie-test-make-adder
                                     trie scopes))
                     (iota thread-count)))
       ;; Check expected number of items in each value.
       (ensure-same (reduce #'+ scopes
                            :key (lambda (scope)
                                   (length (scope-trie-get scope trie))))
                    (* thread-count (length scopes))))))

(addtest (event-processing-scope-trie-root
          :documentation
          "Multi-threaded stress test for the `scope-trie' data
           structure exercising concurrent insertions and deletions.")
  stress.2

  (loop :repeat 1000 :do
     (let ((scopes       *scope-trie-test-lots-of-scopes*)
           (thread-count 3)
           (trie         (make-scope-trie)))
       ;; Run concurrent insertions and deletions.
       (mapc #'bt:join-thread
             (mapcar
              (lambda (i)
                (let ((adder   (%scope-trie-test-make-adder
                                trie scopes i))
                      (remover (%scope-trie-test-make-remover
                                trie scopes i)))
                  (bt:make-thread (lambda ()
                                    (funcall adder)
                                    (funcall remover)))))
              (iota thread-count)))
       ;; Check that all entries have empty values after all
       ;; add-then-remove actions are done.
       (ensure (every (lambda (scope)
                        (null (scope-trie-get scope trie)))
                      scopes)))))

(addtest (event-processing-scope-trie-root
          :documentation
          "Multi-threaded stress test for the `scope-trie' data
           structure exercising concurrent insertions, deletions and
           queries.")
  stress.3

  (let+ ((count 1000000)
         (scope (make-scope '("a" "b" "c" "d" "e")))
         (trie  (let ((trie (make-filled-scope-trie)))
                  (setf (scope-trie-get scope trie)
                        (make-list count :initial-element :foo))
                  trie))
         ((&flet adder ()
            (loop :repeat count :do
               (scope-trie-update
                (lambda (value value?)
                  (values (list* :foo (when value? value)) t))
                scope trie))))
         ((&flet remover ()
            (loop :repeat count :do
               (scope-trie-update
                (lambda (value value?)
                  (let ((new (when value? (rest value))))
                    (values new new)))
                scope trie))))
         ;; "Background" threads concurrently add and remove entries.
         (threads (mapcar #'bt:make-thread (list #'adder #'adder #'remover))))
    (unwind-protect
         ;; Foreground thread queries items at the same time.
         (loop :repeat (* 3 count) :do
            (scope-trie-map #'identity scope trie))
      (mapc #'bt:join-thread threads))
    ;; Check result of concurrent adding and removing.
    (ensure-same (length (scope-trie-get scope trie)) (* 2 count))))

;;; `sink-scope-trie'

(deftestsuite event-processing-sink-scope-trie-root (event-processing-root)
  ()
  (:documentation
   "Test suite for the `sink-scope-trie' class."))

(addtest (event-processing-sink-scope-trie-root
          :documentation
          "Smoke test for the `sink-scope-trie' data structure.")
  smoke

  (let+ ((trie (make-sink-scope-trie))
         ((&flet add-sink (scope)
            (sink-scope-trie-add trie scope :foo)))
         ((&flet remove-sink (scope)
            (sink-scope-trie-remove trie scope :foo))))
    (mapc #'add-sink *scope-trie-test-a-few-scopes*)
    (mapc #'remove-sink *scope-trie-test-a-few-scopes*)))
