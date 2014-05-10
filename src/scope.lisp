;;;; scope.lisp --- Scope class and related functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; `scope' class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass scope ()
    ((components :initarg  :components
                 :type     scope-components
                 :reader   scope-components
                 :documentation
                 "The name components of the scope.")
     (interned?  :initarg  :interned?
                 :type     boolean
                 :accessor scope-interned?
                 :initform nil
                 :documentation
                 "Non-nil if the scope has been interned.")
     (%string    :type     (or null string)
                 :accessor scope-%string
                 :initform nil
                 :documentation
                 "Caches the string representation of the scope."))
    (:default-initargs
     :components (missing-required-initarg 'scope :components))
    (:documentation
     "Instances of this class consist of a hierarchy of zero or more
      names.")))

(defmethod scope-string ((scope scope))
  (or (scope-%string scope)
      (setf (scope-%string scope)
            (with-output-to-string (stream)
              (format stream "/~{~A/~}" (scope-components scope))))))

(defmethod print-object ((object scope) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A~:[~; !~]"
            (scope-string object) (scope-interned? object))))

(defconstant +root-scope+
  (if (boundp '+root-scope+)
      (symbol-value '+root-scope+)
      (make-instance 'scope
                     :components '()
                     :interned?  t))
  "The singleton instance of the root scope.")

;; scope methods

(defmethod make-scope ((thing scope) &key intern?)
  (if intern?
      (intern-scope thing)
      thing))

(defmethod make-scope ((thing null) &key intern?)
  (if intern?
      +root-scope+
      (call-next-method)))

(defmethod make-scope ((thing list) &key intern?)
  (check-type thing scope-components "a list of component strings")

  (make-scope (make-instance 'scope :components thing) :intern? intern?))

(defmethod make-scope ((thing sequence) &key intern?)
  (make-scope (coerce thing 'list) :intern? intern?))

(defmethod make-scope ((thing string) &key intern?)
  (let+ (((&values components separator-count)
          (split-sequence #\/ thing :remove-empty-subseqs t)))
    (when (zerop separator-count)
      (check-type separator-count positive-integer
                  "a positive number of \"/\"-separators"))
    (make-scope components :intern? intern?)))

(declaim (ftype (function (t) (values scope &rest nil)) ensure-scope)
         (inline ensure-scope))
(defun ensure-scope (thing)
  (if (typep thing 'scope)
      thing
      (the (values scope &rest nil) (make-scope thing))))

(declaim (ftype (function (scope scope) (values * &rest nil)) scope=/no-coerce)
         (inline scope=/no-coerce))
(defun scope=/no-coerce (scope1 scope2)
  (or (eq scope1 scope2)
      (and (not (and (scope-interned? scope1) (scope-interned? scope2)))
           (equal (scope-components scope1) (scope-components scope2)))))

(defun scope= (thing1 thing2)
  "Return non-nil if THING1 is the same scope as THING2."
  (scope=/no-coerce (ensure-scope thing1) (ensure-scope thing2)))

(declaim (ftype (function (scope scope) (values * &rest nil)) sub-scope?/no-coerce)
         (inline sub-scope?/no-coerce))
(defun sub-scope?/no-coerce (scope1 scope2)
  (or (eq scope1 scope2)
      (values (starts-with-subseq
               (scope-components scope2) (scope-components scope1)
               :test 'string=))))

(defun sub-scope? (sub super)
  "Return non-nil if SUB is a sub-scope of SUPER."
  (sub-scope?/no-coerce (ensure-scope sub) (ensure-scope super)))

(declaim (ftype (function (scope scope) (values * &rest nil)) super-scope?/no-coerce)
         (inline super-scope?/no-coerce))
(defun super-scope?/no-coerce (super sub)
  (sub-scope?/no-coerce sub super))

(defun super-scope? (super sub)
  "Return non-nil if SUPER is a super-scope of SUB."
  (sub-scope? sub super))

(defun super-scopes (scope
                     &key
                     include-self?)
  "Return the list of superscopes of SCOPE. If INCLUDE-SELF? is
   non-nil, SCOPE is contained in the list. Otherwise, only proper
   superscopes are returned."
  (let ((components (reverse (scope-components scope))))
    (list* +root-scope+
           (iter (for current on (if include-self? components (rest components)))
                 (collect (make-instance 'scope
                                         :components (reverse current))
                   :at :start)))))

(defun merge-scopes (thing1 thing2)
  "Return a `scope' instance that consists of the concatenated
   components of THING1 and THING2."
  (let+ (((&flet make-result (scope1 scope2)
            (make-instance 'scope
                           :components (append (scope-components scope2)
                                               (scope-components scope1))))))
   (cond
     ((eq thing1 +root-scope+)
      (ensure-scope thing2))
     ((eq thing2 +root-scope+)
      (ensure-scope thing1))
     (t
      (make-result (ensure-scope thing1) (ensure-scope thing2))))))

;;; Interning scopes

(defvar *scopes* (let ((table (make-hash-table :test #'equal)))
                   (setf (gethash '() table) +root-scope+)
                   table)
  "Maps scope strings to canonical `scope' instances.")

(defvar *scopes-lock* (bt:make-lock "scopes lock")
  "Protects accesses to `*scopes*' during interning.")

(defun intern-scope (scope)
  "Return the canonical `scope' instance for SCOPE. May return SCOPE,
   if it becomes or already is the canonical instance."
  (if (scope-interned? scope)
      scope
      (let+ (((&values interned found?)
              (bt:with-lock-held (*scopes-lock*)
                (ensure-gethash
                 (scope-components scope) *scopes* scope))))
        (unless found?
          (setf (scope-interned? interned) t))
        interned)))

(defmethod relative-url ((scope scope))
  (make-instance 'puri:uri :path (scope-string scope)))
