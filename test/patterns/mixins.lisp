;;;; mixins.lisp --- Unit tests for mixin classes in the pattern module.
;;;;
;;;; Copyright (C) 2015, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:rsb.patterns.test)

;;; `composite-participant-mixin'

(defclass mock-composite-participant/composite-participant-mixin
    (composite-participant-mixin
     child-container-mixin
     print-items:print-items-mixin)
  ())

(deftestsuite composite-participant-mixin-root (patterns-root)
  ()
  (:documentation
   "Unit tests for the `composite-participant-mixin' class."))

(addtest (composite-participant-mixin-root
          :documentation
          "Smoke test for the `print-items:print-items' method
           specialized on `composite-participant-mixin'.")
  print/smoke

  (let ((participant
         (make-instance 'mock-composite-participant/composite-participant-mixin)))
    (ensure (search "(0)" (princ-to-string participant)))
    (setf (participant-child participant :foo :mock)
          (make-participant :mock "/foo"))
    (ensure (search "(1)" (princ-to-string participant)))))

(addtest (composite-participant-mixin-root
          :documentation
          "Smoke test for the `detach' method specialized on
           `composite-participant-mixin'.")
  detach/smoke

  (let ((parent
         (make-instance 'mock-composite-participant/composite-participant-mixin))
        (child  (make-participant :mock "/foo")))
    (setf (participant-child parent :foo :mock) child)
    (ensure-same (mock-participant-state child) :attached)
    (detach parent)
    (ensure-same (mock-participant-state child) :detached)))

(defclass mock-cleanup-composite-participant (participant
                                              composite-participant-mixin)
  ((children :initarg  :children
             :accessor participant-children)))

(defmethod shared-initialize :after
    ((instance   mock-cleanup-composite-participant)
     (slot-names t)
     &key)
  (error "something went wrong"))

(addtest (composite-participant-mixin-root
          :documentation
          "Ensure that `detach' is called on all already registered
           child participants when an error is signaled during
           initialization.")
  failed-construction-cleanup

  ;; Make sure that `detach' is called for all children that were
  ;; already registered at the time of the error.
  (let ((children (list (make-participant :mock "/compositeparticipantmixinroot/child"))))
    (ensure-condition 'error
      (make-instance 'mock-cleanup-composite-participant
                     :scope    "/compositeparticipantmixinroot"
                     :children children))
    (ensure (every (lambda (child)
                     (eq (mock-participant-state child) :detached))
                   children))))

;;; `child-container-mixin'

(deftestsuite child-container-mixin-root (patterns-root)
  ()
  (:documentation
   "Unit tests for the `child-container-mixin' class."))

(addtest (child-container-mixin-root
          :documentation
          "Smoke test for the `participant-child' and setf
           `participant-child' methods specialized on
           `child-container-mixin'.")
  participant-child/smoke

  (let ((parent (make-instance 'child-container-mixin))
        (child  (make-participant :mock "/foo")))
    (ensure-null (participant-child parent :no-such :no-such))
    (setf (participant-child parent :foo :mock) child)
    (ensure-same (participant-child parent :foo :mock) child)
    (setf (participant-child parent :foo :mock) nil)
    (ensure-null (participant-child parent :foo :mock))))

;;; `configuration-inheritance-mixin'

(defclass mock-composite-participant/configuration-inheritance-mixin
    (configuration-inheritance-mixin
     participant)
  ())

(deftestsuite configuration-inheritance-mixin-root (patterns-root)
  ()
  (:documentation
   "Unit tests for the `configuration-inheritance-mixin' class."))

(addtest (configuration-inheritance-mixin-root
          :documentation
          "Smoke test for the `make-child-initargs' method specialized
           on the `configuration-inheritance-mixin' class.")
  make-child-initargs/smoke

  (ensure-cases (parent-initargs override-initargs expected)
      '(;; Everything in parent initargs, no override initargs.
        ((:transports     ((:socket :enabled t :port 1020))
          :converters     (:foo)
          :transform      1+
          :introspection? t)
         ()
         ((:transports     . ((:inprocess :enabled t)
                              (:socket    :enabled t :port 1020)))
          (:converters     . (:foo))
          (:transform      . 1+)
          (:introspection? . t)))
        ;; Some override initargs.
        ((:transports     ((:socket :enabled t :port 1020))
          :converters     (:foo)
          :transform      1+
          :introspection? t)
         (:transform      1-
          :introspection? nil)
         ((:transports     . ((:inprocess :enabled t)
                              (:socket    :enabled t :port 1020)))
          (:converters     . (:foo))
          (:transform      . 1-)
          (:introspection? . nil))))

    (let* ((class  (find-class 'mock-composite-participant/configuration-inheritance-mixin))
           (parent (progn
                     (c2mop:finalize-inheritance class)
                     (apply #'make-participant-using-class
                            class (c2mop:class-prototype class)
                            (make-scope "/foo") parent-initargs)))
           (result (apply #'make-child-initargs parent :foo :mock
                          override-initargs))
           (result (plist-alist (remove-from-plist result :error-policy))))
      (ensure-same result expected :test (rcurry #'set-equal :test #'equal)))))

(addtest (configuration-inheritance-mixin-root
          :documentation
          "Ensure that the current value of `*configuration*' does not
           affect child participants created using
           `configuration-inheritance-mixin'.")
  no-*configuration*-leakage

  (let* ((class           (find-class 'mock-composite-participant/configuration-inheritance-mixin))
         (parent          (progn
                            (c2mop:finalize-inheritance class)
                            (make-participant-using-class
                             class (c2mop:class-prototype class)
                             (make-scope "/foo"))))
         ;; If `*configuration*' leaks into the
         ;; `make-child-participant' call, an error will be signaled
         ;; since the requested transport does not exist.
         (*configuration* '(((:transport :does-not-exist :enabled) . t))))
    (with-active-participant
        (nil (make-child-participant parent :foo :listener)))))

;;; `lazy-child-making-mixin'

(defclass mock-composite-participant/lazy-child-making-mixin
    (lazy-child-making-mixin
     child-container-mixin
     participant)
  ((args :accessor mock-participant-args)))

(defmethod make-child-participant
    ((participant mock-composite-participant/lazy-child-making-mixin)
     (which       t)
     (kind        t)
     &rest initargs &key)
  (setf (mock-participant-args participant) (list* which kind initargs))
  (make-participant :mock "/foo"))

(deftestsuite lazy-child-making-mixin-root (patterns-root)
  ()
  (:documentation
   "Unit tests for the `lazy-child-making-mixin' class."))

(addtest (lazy-child-making-mixin-root
          :documentation
          "Smoke test for the `participant-child' method specialized
           on `lazy-child-making-mixin'.")
  participant-child/smoke

  (let ((parent (make-instance 'mock-composite-participant/lazy-child-making-mixin
                               :scope "/bar")))
    (participant-child parent :foo :mock :if-does-not-exist :create)
    (ensure-same (mock-participant-args parent) '(:foo :mock))))
