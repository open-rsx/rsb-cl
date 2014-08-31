;;;; mixins.lisp --- Unit tests for mixin classes in the pattern module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
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

  (let* ((class  (find-class 'mock-composite-participant/configuration-inheritance-mixin))
         (parent (progn
                   (c2mop:finalize-inheritance class)
                   (make-participant-using-class
                    class (c2mop:class-prototype class) (make-scope "/foo")
                    :transports     '((:socket :enabled t :port 1020))
                    :converters     '(:foo)
                    :transform      '1+
                    :introspection? t)))
         (result (make-child-initargs parent :foo :mock)))
    (ensure-same (plist-alist (remove-from-plist result :error-policy))
                 '((:transports     . ((:inprocess :enabled t)
                                       (:socket    :enabled t :port 1020)))
                   (:transform      . 1+)
                   (:introspection? . t))
                 :test (rcurry #'set-equal :test #'equal))))

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
