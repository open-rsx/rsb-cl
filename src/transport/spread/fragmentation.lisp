;;;; fragmentation.lisp --- Fragmentation and assembly of data/notifications.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Assembly protocol

(defgeneric add-fragment! (assembly fragment)
  (:documentation
   "Integrate the notification FRAGMENT into the partial assembly
ASSEMBLY. Warning conditions are signaled if FRAGMENT cannot be
integrated for some reason. The (possibly) modified ASSEMBLY is
returned."))

;;; `assembly' class

(defclass assembly ()
  ((id         :initarg  :id
               :type     simple-array
               :reader   assembly-id
               :documentation
               "The id of notifications that are assembled in this
assembly.")
   (birth-time :initarg  :birth-time
               :type     non-negative-real
               :reader   assembly-birth-time
               :initform (internal-real-time-in-seconds)
               :documentation
               "The time in \"internal\" seconds at which this
assembly started.")
   (fragments  :type     vector
               :reader   assembly-fragments
               :documentation
               "Ordered fragments that have been received so far."))
  (:default-initargs
   :num-fragments (missing-required-initarg 'assembly :num-fragments))
  (:documentation
   "Instances of this class represent assembly processes for events
that have been split into multiple notifications."))

(defmethod initialize-instance :after ((instance assembly)
                                       &key
                                       num-fragments)
  (setf (slot-value instance 'fragments)
        (make-array num-fragments :initial-element nil)))

(defun assembly-age (assembly)
  "Return the age of ASSEMBLY in (float) seconds."
  (- (internal-real-time-in-seconds) (assembly-birth-time assembly)))

(defun assembly-complete? (assembly)
  "Return non-nil if all expected fragments have been merged into
ASSEMBLY."
  (notany #'null (assembly-fragments assembly)))

(defun assembly-concatenated-data (assembly)
  "Return an octet-vector containing the concatenated bytes from all
fragments of ASSEMBLY. ASSEMBLY has to be complete."
  (declare #.cl-rsb-system:+optimization-fast+unsafe+)
  (let* ((fragments (map 'list (compose #'notification-data
                                        #'fragmented-notification-notification)
                         (assembly-fragments assembly)))
         (size      (reduce #'+ fragments :key #'length))
         (result    (make-octet-vector size)))
    (iter (for (the simple-octet-vector fragment) in       fragments)
          (for (the fixnum start)                 previous end :initially 0)
          (for (the fixnum end)                   next     (+ start (length fragment)))
          (setf (subseq result start end) fragment))
    result))

(defmethod add-fragment! ((assembly  assembly)
                          (fragment  fragmented-notification))
  (let+ (((&accessors-r/o (fragments assembly-fragments)) assembly)
         ((&accessors-r/o (id fragmented-notification-data-part)) fragment))
    (log:trace "~@<~A is processing fragment ~D~@:>"
               assembly id)
    (cond
      ;; Bounds check for fragment id.
      ((not (<= 0 id (1- (length fragments))))
       (warn 'invalid-fragment-id
             :assembly assembly
             :fragment fragment))

      ;; Check for duplicates.
      ((aref fragments id)
       (warn 'duplicate-fragment
             :assembly assembly
             :fragment fragment))

      ;; Store the fragment.
      (t
       (setf (aref fragments id) fragment))))

  assembly)

(defmethod print-object ((object assembly) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~{~2,'0X~}:~{~2,'0X~} (~D/~D) age ~5,2F s"
            (coerce (subseq (assembly-id object) 4 8) 'list)
            (coerce (subseq (assembly-id object) 0 4) 'list)
            (count-if-not #'null (assembly-fragments object))
            (length (assembly-fragments object))
            (assembly-age object))))

;;; Partial assembly storage protocol

(defgeneric assembly-pool-count (pool)
  (:documentation
   "Return the number of `assembly' instances in POOL."))

(defgeneric ensure-assembly (pool id size)
  (:documentation
   "Find or create an assembly with SIZE total fragments for the event
identified by ID."))

(defgeneric merge-fragment (pool notification)
  (:documentation
   "Merge NOTIFICATION into the appropriate assembly within POOL. If
NOTIFICATION completes the assembly, return a notification instance
built from the complete assembly. Otherwise, return nil."))

;;; Partial assembly storage

(defclass assembly-pool ()
  ((assemblies :type     hash-table
               :reader   assembly-pool-%assemblies
               :initform (make-hash-table :test #'equalp)
               :documentation
               "This hash-table maps event ids to `assembly'
instances."))
  (:documentation
   "Instances of this class create and update `assembly' instances as
necessary when fragments are submitted by calls to
`merge-fragment'."))

(defmethod assembly-pool-count ((pool assembly-pool))
  (hash-table-count (assembly-pool-%assemblies pool)))

(defmethod ensure-assembly ((pool assembly-pool)
                            (id   simple-array)
                            (size integer))
  (let+ (((&accessors-r/o (assemblies assembly-pool-%assemblies)) pool))
    (or (gethash id assemblies)
        (setf (gethash id assemblies)
              (make-instance 'assembly
                             :id            id
                             :num-fragments size)))))

(defmethod merge-fragment ((pool     assembly-pool)
                           (fragment fragmented-notification))
    (let+ (((&accessors-r/o (assemblies assembly-pool-%assemblies)) pool)
           ((&accessors-r/o (notification fragmented-notification-notification)
                            (size         fragmented-notification-num-data-parts)) fragment)
           ((&accessors-r/o (id notification-event-id)) notification)
           (id       (%make-key id))
           (assembly (ensure-assembly pool id size)))
      (when (assembly-complete? (add-fragment! assembly fragment))
        (remhash id assemblies)
        assembly)))

(defmethod print-object ((object assembly-pool) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (assembly-pool-count object))))

;;; Automatic pruning of old incomplete assemblies

(defclass pruning-assembly-pool (assembly-pool)
  ((executor  :accessor assembly-pool-%executor)
   (lock      :reader   assembly-pool-%lock
              :initform (bt:make-lock "Assemblies Lock")
              :documentation
              "This lock protects the collection of `assembly'
instances from concurrent modification by the pruning thread and calls
to `merge-fragment'.")
   (age-limit :initarg  :age-limit
              :type     positive-real
              :accessor assembly-pool-age-limit
              :documentation
              "Controls the minimum age in seconds that `assembly'
instances have to reach before they can be pruned."))
  (:default-initargs
   :age-limit 10)
  (:documentation
   "This instances of this subclass of `assembly-pool' manage a thread
that periodically deletes partial assemblies which are older than
MIN-AGE."))

(defmethod initialize-instance :after ((instance pruning-assembly-pool)
                                       &key
                                       age-limit)
  (setf (assembly-pool-%executor instance)
        (make-instance 'timed-executor/weak
                       :name     (format nil "Executor for ~A" instance)
                       :interval (/ age-limit 4)
                       :function #'delete-partial-assemblies
                       :args     (list instance age-limit))))

(defmethod assembly-pool-count :around ((pool pruning-assembly-pool))
  (bt:with-lock-held ((assembly-pool-%lock pool))
    (call-next-method)))

(defmethod merge-fragment :around ((pool         pruning-assembly-pool)
                                   (notification t))
  (bt:with-lock-held ((assembly-pool-%lock pool))
    (call-next-method)))

(defun delete-partial-assemblies (pool min-age)
  "Find `assembly' instance in POOL whose age is at least MIN-AGE and
delete them."
  (let+ (((&accessors-r/o (assemblies assembly-pool-%assemblies)
                          (lock       assembly-pool-%lock)) pool)
         (string (princ-to-string pool)))
    (bt:with-lock-held (lock)
      (when-let ((old (remove min-age (hash-table-values assemblies)
                              :test #'>=
                              :key  #'assembly-age)))
        (log:info "~@<~A is removing ~D partial assembl~:@P~@:>"
                  string (length old))
        (iter (for assembly in old)
              (remhash (assembly-id assembly) assemblies))))))

;;; Fragmentation

(declaim (ftype (function (simple-octet-vector non-negative-fixnum non-negative-fixnum)
                          simple-octet-vector)
                make-data-fragment)
         (inline make-data-fragment))

(defun make-data-fragment (data offset chunk-size)
  "Return a chunk of length CHUNK-SIZE from DATA starting at OFFSET."
  (subseq data offset (+ offset chunk-size)))

;;; Utility functions

(defun %make-key (event-id)
  "Return a vector that can be used to identify the notification from
which SEQUENCE-NUMBER and SENDER-ID have been extracted."
  (concatenate 'simple-octet-vector
               (nth-value 1 (binio:encode-uint32-be
                             (event-id-sequence-number event-id)))
               (event-id-sender-id event-id)))
