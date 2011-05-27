;;; fragmentation.lisp --- Fragmentation and assembly of data/notifications.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.transport.spread)


;;; `assembly' class
;;

(defclass assembly ()
  ((id         :initarg  :id
	       :type     string
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
  (let* ((fragments (map 'list (compose #'rsb.protocol::attachment-binary
					#'rsb.protocol::notification-data)
			 (assembly-fragments assembly)))
	 (size      (reduce #'+ fragments :key #'length))
	 (result    (make-array size :element-type '(unsigned-byte 8))))
    (iter (for (the octet-vector fragment) in       fragments)
	  (for (the fixnum start)          previous end :initially 0)
	  (for (the fixnum end)            next     (+ start (length fragment)))
	  (setf (subseq result start end) fragment))
    result))

(defmethod add-fragment! ((assembly assembly)
			  (fragment rsb.protocol::notification))
  "Integrate the notification FRAGMENT into the partial assembly
ASSEMBLY. Warning conditions are signaled if FRAGMENT cannot be
integrated for some reason. The (possibly) modified ASSEMBLY is
returned."
  (bind (((:accessors-r/o (fragments assembly-fragments)) assembly)
	 ((:accessors-r/o
	   (id rsb.protocol::notification-data-part)) fragment))
    (log1 :info "~S processing fragment ~S" assembly id)
    (cond
      ;; Bounds check for fragment id.
      ((not (<= 0 id (1- (length fragments))))
       (warn "~@<Received illegal fragment ~D of ~S~@:>"
	     id assembly))

      ;; Check for duplicates.
      ((aref fragments id)
       (warn "~@<Received fragment ~D of ~S more than once~@:>"
	     id assembly)) ;; TODO proper condition

      ;; Store the fragment.
      (t
       (setf (aref fragments id) fragment))))

  assembly)

(defmethod print-object ((object assembly) stream)
  (with-slots (fragments) object
    (print-unreadable-object (object stream :type t)
      (format stream "~:@(~8,'0A~) (~D/~D) age ~5,2F s"
	      (subseq (assembly-id object) 0 8)
	      (count-if-not #'null fragments) (length fragments)
	      (assembly-age object)))))


;;; Partial assembly storage protocol
;;

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
;;

(defclass assembly-pool ()
  ((assemblies :type     hash-table
	       :initform (make-hash-table :test #'equal)
	       :documentation
	       "This hash-table maps event ids to `assembly'
instances."))
  (:documentation
   "Instances of this class create and update `assembly' instances as
necessary when fragments are submitted by calls to
`merge-fragment'."))

(defmethod assembly-pool-count ((pool assembly-pool))
  (hash-table-count (slot-value pool 'assemblies)))

(defmethod ensure-assembly ((pool assembly-pool)
			    (id   string)
			    (size integer))
  (bind (((:slots-r/o assemblies) pool))
    (or (gethash id assemblies)
	(setf (gethash id assemblies)
	      (make-instance 'assembly
			     :id            id
			     :num-fragments size)))))

(defmethod merge-fragment ((pool         assembly-pool)
			   (notification t))
    (bind (((:slots-r/o assemblies) pool)
	   ((:accessors-r/o
	     (id rsb.protocol::notification-id)
	     (size rsb.protocol::notification-num-data-parts))
	    notification))
      (let ((assembly (ensure-assembly pool id size)))
	(when (assembly-complete? (add-fragment! assembly notification))
	  (remhash id assemblies)
	  assembly))))

(defmethod print-object ((object assembly-pool) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (assembly-pool-count object))))


;;; Automatic pruning of old incomplete assemblies
;;

(defclass pruning-assembly-pool (assembly-pool)
  ((lock      :initform (bt:make-lock "Assemblies Lock")
	      :documentation
	      "This lock protects the collection of `assembly'
instances from concurrent modification by the pruning thread and calls
to `merge-fragment'.")
   (thread    :type     bt:thread
	      :documentation
	      "Stores the thread in the context of which the pruning
of incomplete assemblies is done. ")
   (stop?     :type     boolean
	      :initform nil
	      :documentation
	      "This flag controls termination of the pruning thread.")
   (age-limit :initarg  :age-limit
	      :type     positive-real
	      :accessor assembly-pool-age-limit
	      :initform 10
	      :documentation
	      "Controls the minimum age in seconds that `assembly'
instances have to reach before they can be pruned."))
  (:documentation
   "This instances of this subclass of `assembly-pool' manage a thread
that periodically deletes partial assemblies which are older than
MIN-AGE."))

(defmethod initialize-instance :after ((instance pruning-assembly-pool)
                                       &key)
  ;; Create a thread that periodically deletes partial assemblies.
  (setf (slot-value instance 'thread)
	(bt:make-thread
	 #'(lambda ()
	     (iter (until (slot-value instance 'stop?))
		   (let ((age-limit (assembly-pool-age-limit instance)))
		     (delete-partial-assemblies
		      instance age-limit)
		     (sleep (/ age-limit 4)))))))

  ;; Terminate the thread that deletes partial assemblies.
  (tg:finalize instance #'(lambda ()
			    (with-slots (thread stop?) instance
			      (setf stop? t)
			      (bt:join-thread thread)))))

(defmethod assembly-pool-count :around ((pool pruning-assembly-pool))
  (bt:with-lock-held ((slot-value pool 'lock))
    (call-next-method)))

(defmethod merge-fragment :around ((pool         pruning-assembly-pool)
				   (notification t))
  (bt:with-lock-held ((slot-value pool 'lock))
    (call-next-method)))

(defun delete-partial-assemblies (pool min-age)
  "Find `assembly' instance in POOL whose age is at least MIN-AGE and
delete them."
  (bind (((:slots-r/o assemblies lock) pool))
    (bt:with-lock-held (lock)
      (let ((old (remove min-age (hash-table-values assemblies)
			 :test #'>=
			 :key  #'assembly-age)))
	(when old
	  (log1 :info "~S Removing partial assemblies ~_~{~S~^, ~}" pool old)
	  (iter (for assembly in old)
		(remhash (assembly-id assembly) assemblies)))))))


;;; Fragmentation
;;

(defun fragment-data (data chunk-size)
  "Partition DATA into chunks of at most CHUNK-SIZE bytes. Return a
list of the generated chunks."
  (check-type data octet-vector "An octet-vector")

  (iter (for offset :from 0 :by chunk-size)
	(while (< offset (length data)))
	(for size next (min chunk-size (- (length data) offset)))
	(collect
	    (make-array size
			:element-type           (array-element-type data)
			:displaced-to           data
			:displaced-index-offset offset))))
