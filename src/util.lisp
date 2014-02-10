;;;; util.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Sequence number functions

(declaim (ftype (function (&optional sequence-number)
                          (function () sequence-number))
                make-sequence-number-generator))

(defun make-sequence-number-generator (&optional (start 0))
  "Return a function that returns increasing numbers starting with
START."
  #.(if (subtypep 'sequence-number 'lparallel.counter:counter-value)
        '(let ((current (lparallel.counter:make-counter start)))
           (lambda ()
             (declare #.+optimization-fast+unsafe+)
             (mod (lparallel.counter:inc-counter current)
                  (ash 1 32))))
        #+sbcl
        ;; We have to wrap the value in a cons.
        '(let ((current (list (list start))))
           (declare (type (cons (cons sequence-number null) null) current))
           (lambda ()
             (declare #.+optimization-fast+unsafe+)
             ;; Keep trying to store the incremented value until it
             ;; works.
             (iter (for old next (car current))
                   (for new next (list (ldb (byte 32 0)
                                            (1+ (the sequence-number
                                                     (car old))))))
                   (until (eq old (sb-ext:compare-and-swap
                                   (car current) old new)))
                   (finally (return (car old))))))
        #-sbcl
        '(error "Not implemented")))

;;; UUID utility functions

(defun print-id (stream id colon? at?)
  "Print the UUID ID to STREAM. If COLON? is non-nil, all components
of ID are printed. Otherwise, just a block of 8 digits is printed.  "
  (declare (ignore at?))
  (cond
    ((null id)
     (format stream "    NOID"))
    ((or colon? (not (typep id 'uuid:uuid)))
     (format stream "~A" id))
    (t
     (format stream "~8,'0X" (slot-value id 'uuid::time-low)))))

(defmacro print-unreadable-id-object ((object stream &key (type t))
                                      &body body)
  "Print OBJECT to STREAM in a manner similar to
`print-unreadable-object' but use the `id' slot of OBJECT as object
identity."
  (once-only (object stream)
    (with-unique-names (id-var)
      `(let ((,id-var (slot-value ,object 'id)))
         (print-unreadable-object (,object ,stream :type ,type)
           ,@body
           (write-char #\Space stream)
           (print-id ,stream ,id-var nil nil))))))

;;; Utility functions

(defun maybe-shorten-sequence (thing)
  (if (typep thing 'sequence)
      (let ((length (length thing)))
        (values (subseq thing 0 (min length 200)) (> length 200)))
      (values thing nil)))

;;; plist-mixin

(defmacro define-plist-data-mixin (name
                                   &key
                                   (slot-name name))
  "Define a class `plist-NAME-mixin' which manages a plist in a
   slot. Define the following accessors along with the class:

   `NAME-count'

     Return number of items.

   `NAME-keys'

     Return item keys.

   `NAME-values'

     Return item values.

   `NAME-plist'

     Return items as plist.

   `NAME-alist'

     Return items as alist."
  (let+ ((class-name (symbolicate "PLIST-" name "-MIXIN"))
         (initarg    (make-keyword slot-name))
         ((count-name keys-name values-name plist-name alist-name)
          (map 'list (curry #'symbolicate name)
               '("-COUNT" "-KEYS" "-VALUES" "-PLIST" "-ALIST"))))
    `(progn
       (defclass ,class-name ()
         ((,slot-name :initarg  ,initarg
                      :type     list
                      :initform nil
                      :documentation
                      ,(format nil "Stores the ~(~A~) items associated ~
                                    to the instance."
                               name)))
         (:documentation
          "This mixin adds storage for a plist of items and associated
           accessors. See `define-plist-data-mixin' for a description."))

       (defgeneric ,count-name (object)
         (:method ((object ,class-name))
           (/ (length (slot-value object ',slot-name)) 2))
         (:documentation
          ,(format nil "Return the number of ~(~A~) items stored in OBJECT."
                   name)))

       (defgeneric ,keys-name (object)
         (:method ((object ,class-name))
           (iter (for (key) on (slot-value object ',slot-name)
                      :by #'cddr)
                 (collect key)))
         (:documentation
          ,(format nil "Return a list of the keys of ~(~A~) items ~
                        stored in OBJECT."
                   name)))

       (defgeneric ,values-name (object)
         (:method ((object ,class-name))
           (iter (for (key value) on (slot-value object ',slot-name)
                      :by #'cddr)
                 (collect value)))
         (:documentation
          ,(format nil "Return a list of the values of ~(~A~) items ~
                        stored in OBJECT."
                   name)))

       (defgeneric ,plist-name (object)
         (:method ((object ,class-name))
           (slot-value object ',slot-name))
         (:documentation
          ,(format nil "Return a plist of the ~(~A~) items stored in ~
                        OBJECT."
                   name)))

       (defgeneric ,alist-name (object)
         (:method ((object ,class-name))
           (plist-alist (slot-value object ',slot-name)))
         (:documentation
          ,(format nil "Return an alist of the ~(~A~) items stored ~
                        in OBJECT."
                   name)))

       (defgeneric ,name (object key)
         (:method ((object ,class-name) (key t))
           (getf (slot-value object ',slot-name) key))
         (:documentation
          ,(format nil "Return the ~(~A~) item of OBJECT identified ~
                        by KEY."
                   name)))

       (defgeneric (setf ,name) (new-value object key)
         (:method ((new-value t) (object ,class-name) (key t))
           (setf (getf (slot-value object ',slot-name) key) new-value))
         (:documentation
          ,(format nil "Associate NEW-VALUE to OBJECT as the ~(~A~)
                        item identified by KEY."
                   name))))))
