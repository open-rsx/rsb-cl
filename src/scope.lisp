;;;; scope.lisp --- Scope class and related functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Scope protocol

(defgeneric scope-components (scope)
  (:documentation
   "Return a list containing the components of SCOPE."))

(defgeneric scope-string (scope)
  (:documentation
   "Return a string representation of SCOPE."))

(defgeneric make-scope (thing
                        &key
                        intern?)
  (:documentation
   "Parse string and return a `scope' instance."))

(declaim (ftype (function (scope) (values scope &optional)) intern-scope))

;;; `scope' class

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
               :accessor %scope-string
               :initform nil
               :documentation
               "Caches the string representation of the scope."))
  (:default-initargs
   :components (missing-required-initarg 'scope :components))
  (:documentation
   "Instances of this class consist of a hierarchy of zero or more
names."))

(defmethod scope-string ((scope scope))
  (or (%scope-string scope)
      (setf (%scope-string scope)
            (with-output-to-string (stream)
              (format stream "/~{~A/~}" (scope-components scope))))))

(defmethod print-object ((object scope) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A~:[~; !~]"
            (scope-string object) (scope-interned? object))))

;;;

(defmethod make-scope ((thing scope)
                       &key
                       intern?)
  (if intern?
      (intern-scope thing)
      thing))

(defmethod make-scope ((thing list)
                       &key
                       intern?)
  (check-type thing scope-components "a list of component strings")

  (make-scope (make-instance 'scope
                             :components thing)
              :intern? intern?))

(defmethod make-scope ((thing sequence)
                       &key
                       intern?)
  (make-scope (coerce thing 'list) :intern? intern?))

(defmethod make-scope ((thing string)
                       &key
                       intern?)
  (let+ (((&values components separator-count)
          (split-sequence #\/ thing :remove-empty-subseqs t)))
    (when (zerop separator-count)
      (check-type separator-count positive-integer
                  "a positive number of \"/\"-separators"))
    (make-scope components :intern? intern?)))

(defun scope= (thing1 thing2)
  "Return non-nil if THING1 is the same scope as THING2."
  (let ((scope1 (make-scope thing1))
        (scope2 (make-scope thing2)))
    (equal (scope-components scope1) (scope-components scope2))))

(defun sub-scope? (sub super)
  "Return non-nil if SUB is a sub-scope of SUPER."
  (let ((scope1 (make-scope sub))
        (scope2 (make-scope super)))
    (values
     (starts-with-subseq (scope-components scope2)
                         (scope-components scope1)
                         :test 'string=))))

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
    (cons (make-scope nil)
          (iter (for current on (if include-self? components (rest components)))
                (collect (make-instance 'scope
                                        :components (reverse current))
                  :at :start)))))

(defun merge-scopes (thing1 thing2)
  "Return a `scope' instance that consists of the concatenated
components of THING1 and THING2."
  (let ((scope1 (make-scope thing1))
        (scope2 (make-scope thing2)))
    (make-instance 'scope
                   :components (append (scope-components scope2)
                                       (scope-components scope1)))))

;;; Interning scopes

(defvar *scopes* (make-hash-table :test #'equal)
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
  (make-instance 'puri:uri
                 :path (scope-string scope)))
