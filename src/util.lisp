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

(defun print-id (stream id &optional colon? at?)
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
    `(print-unreadable-object (,object ,stream :type ,type)
       ,@body
       (write-char #\Space stream)
       (print-id ,stream (slot-value ,object 'id)))))

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

;;; `timed-executor'

(defun %maybe-unschedule-timer (timer)
  (when (sb-ext:timer-scheduled-p timer)
    (log:info "~@<Unscheduling timer ~A~@:>" timer)
    (sb-ext:unschedule-timer timer)))

(defclass timed-executor (print-items:print-items-mixin)
  ((timer    :accessor executor-%timer
             :documentation
             "Stores the timer used to trigger execution of the target
              function.")
   (interval :initarg  :interval
             :type     positive-real
             :accessor executor-interval
             :documentation
             "Stores the time in seconds between successive calls of
              the target function."))
  (:default-initargs
   :interval (missing-required-initarg 'timed-executor :interval)
   :function (missing-required-initarg 'timed-executor :function))
  (:documentation
   "Instances of this class repeatedly execute given functions at
    regular intervals.

    A finalizer is installed to unschedule the execution of the
    function when the executor instances becomes garbage."))

(defmethod initialize-instance :after
    ((instance timed-executor)
     &key
     (name               "Timed executor")
     (interval           nil              interval?)
     function
     args
     (effective-function (lambda ()
                           (values (apply function args) t))))
  (declare (type function effective-function))
  (let+ ((timer-cell (list nil))
         ((&flet finalize ()
            (when-let ((timer (car timer-cell)))
              (%maybe-unschedule-timer timer))))
         ((&flet execute ()
            (unless (lastcar (multiple-value-list
                              (funcall effective-function)))
              (finalize))))
         (timer (sb-ext:make-timer #'execute
                                   :name   name
                                   :thread t)))
    (setf (car timer-cell)           timer
          (executor-%timer instance) timer)
    (tg:finalize instance #'finalize))

  (when interval?
    (setf (executor-interval instance) interval)))

(defmethod (setf executor-interval) :after ((new-value real)
                                            (thing     timed-executor))
  (let+ (((&structure-r/o executor- (timer %timer)) thing))
    (%maybe-unschedule-timer timer)
    (sb-ext:schedule-timer timer new-value :repeat-interval new-value)))

(defmethod print-items:print-items append ((object timed-executor))
  (let+ (((&structure-r/o executor- (timer %timer) interval) object))
    `((:name            ,(sb-ext:timer-name timer) "~S")
      (:update-interval ,interval                  " ~,3F s" ((:after :name))))))

(defmethod detach ((participant timed-executor))
  (tg:cancel-finalization participant)
  (%maybe-unschedule-timer (executor-%timer participant)))

;;; `timed-executor/weak'

(defun curry/weak (function &rest args)
  "Like `alexandria:curry', return a function representing the partial
   application of FUNCTION to ARGS, but use weak references to the
   elements of ARGS.

   The returned function behaves as follows w.r.t. ARGS:

   * If it is called while all elements of ARGS are still reachable,
     FUNCTION is called with those arguments and two values are
     returned: 1) the result of calling function 2) t.

   * If it is called after one or more elements of ARGS have become
     garbage, FUNCTION is not called and two nil values are returned."
  (let ((function (coerce function 'function)))
    (unless args
      (return-from curry/weak
        (lambda (&rest new-args)
          (values (apply function new-args) t))))

    (let ((args/weak (mapcar #'tg:make-weak-pointer args)))
      (named-lambda call-when-reachable (&rest new-args)
        (let+ (((&flet value-or-return (weak-pointer)
                  (or (tg:weak-pointer-value weak-pointer)
                      (return-from call-when-reachable (values nil nil)))))
               (args (mapcar #'value-or-return args/weak)))
          (declare (dynamic-extent args))
          (values (apply function (nconc args new-args)) t))))))

(defclass timed-executor/weak (timed-executor)
  ()
  (:documentation
   "Like `timed-executor' but only keep weak references to the
    arguments of the function.

    As a result, this executor can be used as follows:

      (let* ((my-object   ...)
             (my-executor (make-instance 'timed-executor/weak
                                         :function #'my-object-slot
                                         :args     (list my-object))))
        ...)

    with the effect that `my-executor' will stop calling
    `my-object-slot' when `my-object' becomes garbage (Like
    `timed-executor', the executor unschedules itself when it becomes
    garbage)."))

(defmethod initialize-instance :around ((instance timed-executor/weak)
                                        &rest args1 &key
                                        function
                                        args)
  (apply #'call-next-method instance
         :effective-function (apply #'curry/weak function args)
         (remove-from-plist args1 :function :args)))
