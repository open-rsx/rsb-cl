;;;; scope.lisp --- Scope class and related functions.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; `scope' class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass scope ()
    ((components :initarg  :components
                 :type     list ; actually scope-components
                 :reader   scope-components
                 :documentation
                 "The name components of the scope.")
     (interned?  :initarg  :interned?
                 :type     boolean
                 :accessor scope-interned?
                 :initform nil
                 :documentation
                 "Non-nil if the scope has been interned.")
     (%string    :initarg  :%string
                 :type     (or null string)
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
              (write-char #\/ stream)
              (dolist (component (scope-components scope))
                (write-string component stream)
                (write-char #\/ stream))))))

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

(defmethod make-scope ((thing t) &key intern?)
  (declare (ignore intern?))
  (error 'type-error
         :datum         thing
         :expected-type '(or scope string scope-components)))

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

  (let ((scope (make-instance 'scope :components thing)))
    (if intern?
        (intern-scope scope)
        scope)))

(defmethod make-scope ((thing sequence) &key intern?)
  (make-scope (coerce thing 'list) :intern? intern?))

(defun starts-with-/ (string)
  (starts-with #\/ string))

(defmethod make-scope ((thing string) &key intern?)
  (declare (type (not (simple-array nil)) thing))
  (when (or (zerop (length thing)) (char/= (aref thing 0) #\/))
    (scope-parse-error
     thing 0 "~@<\"~A\" does not start with a \"/\".~@:>" thing))

  (let ((scope
         (loop :with string = thing
            :with length = (length thing)
            :with from :of-type array-index = 1
            :for to :of-type array-index :from 1 :below length
            :for character = (aref thing to)

            :if (char= character #\/)
            :if (>= from to)
            :do (setf from (1+ to) string nil)
            :else :collect (subseq thing from to) :into components
            :and :do (setf from (1+ to))
            :else
            :unless (scope-component-character? character)
            :do (scope-parse-error
                 thing to
                 "~@<The character ~A (~A) at position ~D is illegal.~@:>"
                 character (char-name character) to)
            :when (and (= to (1- length)) (> length from))
            :collect (subseq thing from length) :into components
            :and do (setf string nil)

            :finally (return (make-instance 'scope
                                            :components components
                                            :%string    string)))))
    (if intern?
        (intern-scope scope)
        scope)))

(declaim (ftype (function (t) (values scope &optional)) ensure-scope)
         (inline ensure-scope))
(defun ensure-scope (thing)
  (if (typep thing 'scope)
      thing
      (the (values scope &optional) (make-scope thing))))

(declaim (ftype (function (scope scope) (values * &optional)) scope=/no-coerce)
         (inline scope=/no-coerce))
(defun scope=/no-coerce (scope1 scope2)
  (or (eq scope1 scope2)
      (and (not (and (scope-interned? scope1) (scope-interned? scope2)))
           (equal (scope-components scope1) (scope-components scope2)))))

(defun scope= (thing1 thing2)
  "Return non-nil if THING1 is the same scope as THING2."
  (scope=/no-coerce (ensure-scope thing1) (ensure-scope thing2)))

(declaim (ftype (function (scope scope) (values * &optional)) sub-scope?/no-coerce)
         (inline sub-scope?/no-coerce))
(defun sub-scope?/no-coerce (scope1 scope2)
  (or (eq scope1 scope2)
      (values (starts-with-subseq
               (scope-components scope2) (scope-components scope1)
               :test 'string=))))

(defun sub-scope? (sub super)
  "Return non-nil if SUB is a sub-scope of SUPER."
  (sub-scope?/no-coerce (ensure-scope sub) (ensure-scope super)))

(declaim (ftype (function (scope scope) (values * &optional)) super-scope?/no-coerce)
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
  (cond
    ((eq thing1 +root-scope+)
     (ensure-scope thing2))
    ((eq thing2 +root-scope+)
     (ensure-scope thing1))
    (t
     (make-instance
      'scope
      :components (append (scope-components (ensure-scope thing2))
                          (scope-components (ensure-scope thing1)))))))

(defun enough-scope (thing &optional (defaults +root-scope+))
  "Return a `scope' that yields THING when merged with DEFAULTS.

   That is, if THING is a sub-scope of DEFAULTS, the following
   invariant holds:

     (merge-scopes (enough-scope thing defaults) defaults) ≡ thing

   If THING is a sub-scope of DEFAULTS an error is signaled."
  (cond
    ((eq thing +root-scope+)
     thing)
    ((eq defaults +root-scope+)
     (ensure-scope thing))
    (t
     (let* ((thing              (ensure-scope thing))
            (thing-components   (scope-components thing))
            (defaults           (ensure-scope defaults))
            (default-components (scope-components defaults))
            (mismatch           (mismatch thing-components default-components
                                          :test #'string=)))
       (cond
         ((not mismatch)
          +root-scope+)
         ((length= 0 default-components)
          thing)
         ((< mismatch (length default-components))
          (error "~@<~A is not a sub-scope of ~A.~@:>"
                 thing defaults))
         (t
          (make-instance
           'scope
           :components (subseq thing-components mismatch))))))))

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
      (bt:with-lock-held (*scopes-lock*)
        (ensure-gethash (scope-components scope) *scopes*
                        (progn
                          (scope-string scope) ; force cache
                          (setf (scope-interned? scope) t)
                          scope)))))

(defmethod relative-url ((scope scope))
  (make-instance 'puri:uri :path (scope-string scope)))
