;;;; mixins.lisp --- Mixins for introspection-related classes.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; `introspection-participant-mixin'

(defclass introspection-participant-mixin (participant
                                           composite-participant-mixin
                                           lazy-child-making-mixin
                                           child-container-mixin
                                           configuration-inheritance-mixin)
  ()
  (:default-initargs
   :scope +introspection-scope+)
  (:documentation
   "This class is intended to be mixed into introspection participant
    classes that have to send and receive events."))

(defmethod make-child-initargs ((participant introspection-participant-mixin)
                                (which       t)
                                (kind        t)
                                &key)
  (list* :introspection? nil
         :converters     *introspection-all-converters*
         (call-next-method)))

(defmethod make-child-scope ((participant introspection-participant-mixin)
                             (which       (eql :participants))
                             (kind        t))
  (introspection-participants-scope (participant-scope participant)))

;;; `participant-table-mixin'

(defclass participant-table-mixin ()
  ((participants :type     hash-table
                 :accessor introspection-%participants
                 :initform (make-hash-table :test #'equalp)
                 :documentation
                 "Stores a mapping of participant ids to
                  `participant-info' instances."))
  (:documentation
   "This mixin class adds a table of `participant-info' instances
    indexed by id."))

(defmethod print-items:print-items append ((object participant-table-mixin))
  `((:num-participants ,(length (introspection-participants object)) " (~D)")))

(defmethod introspection-participants ((container participant-table-mixin))
  (hash-table-values (introspection-%participants container)))

(defmethod (setf introspection-participants) ((new-value sequence)
                                              (container participant-table-mixin))
  (map nil (lambda (participant)
             (setf (find-participant (participant-id participant) container)
                   participant))
       new-value))

(defmethod find-participant ((id        uuid:uuid)
                             (container participant-table-mixin)
                             &key
                             parent-id
                             (if-does-not-exist #'error))
  (declare (ignore parent-id))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (or (gethash key %participants)
        (error-behavior-restart-case
          (if-does-not-exist
           (no-such-participant-error :container container :id id))))))

(defmethod (setf find-participant) ((new-value t)
                                    (id        uuid:uuid)
                                    (container participant-table-mixin)
                                    &key
                                    parent-id
                                    if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (setf (gethash key %participants) new-value)))

(defmethod (setf find-participant) ((new-value (eql nil))
                                    (id        uuid:uuid)
                                    (container participant-table-mixin)
                                    &key
                                    parent-id
                                    (if-does-not-exist #'error))
  (declare (ignore parent-id))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (if (gethash key %participants)
        (remhash key %participants)
        (error-behavior-restart-case
            (if-does-not-exist
             (simple-error ; TODO condition
              :format-control   "~@<Cannot remove unknown participant ~
                                 with id ~A~@:>"
              :format-arguments (list id))
             :warning-condition simple-warning)))))

(defmethod ensure-participant ((id          uuid:uuid)
                               (container   participant-table-mixin)
                               (participant t))
  (or (find-participant id container :if-does-not-exist nil)
      (setf (find-participant id container) participant)))

(defmethod ensure-participant ((id          uuid:uuid)
                               (container   participant-table-mixin)
                               (participant cons)) ; (CLASS . INITARGS)
  (let ((existing (find-participant id container :if-does-not-exist nil)))
    (typecase existing
      (null
       (setf (find-participant id container)
             (apply #'make-instance participant)))
      (participant-entry-proxy
       (setf (find-participant id container)
             (apply #'change-class existing participant)))
      (t
       existing))))

;;; `lockable-database-mixin'

(defclass lockable-database-mixin ()
  ((lock :reader   database-%lock
         :initform (bt:make-lock "Database lock")))
  (:documentation
   "This mixins add a lock for use in introspection database
    classes."))

(defmethod call-with-database-lock ((database lockable-database-mixin)
                                    (thunk    function))
  (bt:with-lock-held ((database-%lock database))
    (funcall thunk)))

(defmacro with-database-lock ((database) &body body)
  `(call-with-database-lock ,database (lambda () ,@body)))

;;; `change-hook-mixin'

(defclass change-hook-mixin ()
  ((change-hook :type     list
                :initform '()
                :documentation
                "Stores a list of handlers to run when something
                 changes in the object."))
  (:documentation
   "This class is intended to be mixed into database classes that
    notify handlers of changes."))

(defmethod database-change-hook ((database change-hook-mixin))
  (hooks:object-hook database 'change-hook))
