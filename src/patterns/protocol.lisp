;;;; protocol.lisp --- Protocol provided by the patterns module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns)

;;; Participant children protocol
;;;
;;; Allows retrieving the list of child participants of a given
;;; composite participant.

(defgeneric participant-children (participant)
  (:documentation
   "Return the list of child participants of PARTICIPANT."))

;;; Child participant lookup protocol
;;;
;;; Allows reading and writing child participants of a given
;;; participant according to their respective kind and unique name.

(defgeneric participant-child (participant which kind
                               &key
                               if-does-not-exist
                               if-exists
                               detach?)
  (:documentation
   "Return the child participant of PARTICIPANT uniquely identified by
    WHICH and KIND.

    KIND is a participant kind. The returned participant has to be of
    kind KIND.

    WHICH (in combination with KIND) uniquely identifies the requested
    child participant within PARTICIPANT.

    IF-DOES-NOT-EXIST controls the behavior in case the requested
    child participant does not exist in PARTICIPANT. The default
    behavior is returning nil. The following values are accepted:

    error | (function error)

      Signal a `no-such-child-error'. A `use-value' restart for
      returning a given value instead of the missing child participant
      is established when the error is signaled.

    <any other object>

      Return that object.

    IF-EXISTS and DETACH? are accepted but ignored for parity with
    `(setf participant-child)'"))

(defgeneric (setf participant-child) (new-value participant which kind
                                      &key
                                      if-does-not-exist
                                      if-exists
                                      detach?)
  (:documentation
   "Install NEW-VALUE as the child participant of PARTICIPANT uniquely
    identified by WHICH and KIND.

    KIND is a participant kind.

    WHICH (in combination with KIND) uniquely identifies the new child
    participant within PARTICIPANT.

    NEW-VALUE has to be a participant of kind KIND.

    IF-DOES-NOT-EXIST is accepted but ignored for parity with
    `participant-child'.

    IF-EXISTS controls the behavior in case a child identified by
    WHICH and KIND already exists in PARTICIPANT. The default behavior
    is replacing the existing child. The following values are accepted:

    error | (function error)

      Signal a `child-exists-error'. A `continue' restart for
      replacing the existing child participant is established when the
      error is signaled.

    :supersede

      Replace the existing child participant with NEW-VALUE.

    DETACH? controls whether an existing child participant is detached
    when replaced."))

;; Default behavior

(defmethod participant-child :around ((participant t)
                                      (which       t)
                                      (kind        t)
                                      &key
                                      if-does-not-exist
                                      if-exists
                                      detach?)
  (declare (ignore if-exists detach?))
  (or (call-next-method)
      (error-behavior-restart-case
          (if-does-not-exist (no-such-child-error
                              :container participant
                              :key       (list kind which)))
        (use-value (value)
          :report (lambda (stream)
                    (format stream "~@<Supply a value to use in place ~
                                    of the missing child participant ~
                                    designated by ~S ~S of participant ~
                                    ~A~@:>"
                            which kind participant))
          value))))

(defmethod (setf participant-child) :around ((new-value   t)
                                             (participant t)
                                             (which       t)
                                             (kind        t)
                                             &key
                                             if-does-not-exist
                                             (if-exists        :supersede)
                                             (detach?          t))
  (declare (ignore if-does-not-exist))
  (when-let ((existing (and new-value
                            (participant-child participant which kind
                                               :if-does-not-exist nil))))
    (ecase
        (error-behavior-restart-case
            (if-exists (child-exists-error
                        :container participant
                        :key       (list kind which)
                        :child     existing))
          (continue (&optional condition)
            :report (lambda (stream)
                      (format stream "~@<Replace the existing child ~
                                      participant ~A designated by ~S ~
                                      ~S of participant ~A by ~A.~@:>"
                              existing which kind participant new-value))
            (declare (ignore condition))
            :supersede))
      (:supersede
       (unless (eq existing new-value)
         (setf (participant-child participant which kind :detach? detach?)
               nil)))))
  (call-next-method))

;;; Child participant creation protocol
;;;
;;; Allows creating child participants of a given participant
;;; according to their respective kind and unique name.
;;;
;;; The entry point of the protocol is the `make-child-participant'
;;; generic function which by default calls `make-child-scope' and
;;; `make-child-initargs' to assemble the necessary information to
;;; actually construct child participants.

(defgeneric make-child-scope (participant which kind)
  (:documentation
   "Return a suitable `scope' for the child participant of PARTICIPANT
    identified by KIND and WHICH.

    The default behavior consists in deriving a `scope-component' from
    WHICH and forming a sub-scope by merging the scope component with
    the scope of PARTICIPANT."))

(defgeneric make-child-initargs (participant which kind
                                 &rest args &key &allow-other-keys)
  (:documentation
   "Return a suitable list of initargs for the child participant of
    PARTICIPANT identified by KIND and WHICH.

    ARGS are the explicitly supplied initargs (e.g. supplied in the
    `make-child-participant' call that called this function).

    The default behavior consists in returning ARGS."))

(defgeneric make-child-participant (participant which kind
                                    &rest args &key &allow-other-keys)
  (:documentation
   "Create and return the child participant of PARTICIPANT identified
    by WHICH and KIND.

    The default behavior consists in first calling `make-child-scope'
    and `make-child-initargs' and then calling `make-participant' with
    the collected information."))

;; Default behavior

(defmethod make-child-scope ((participant t)
                             (which       (eql nil))
                             (kind        t))
  (participant-scope participant))

(defmethod make-child-scope ((participant t)
                             (which       symbol)
                             (kind        t))
  (make-child-scope participant (string which) kind))

(defmethod make-child-scope ((participant t)
                             (which       string)
                             (kind        t))
  (let ((scope (make-scope (list (derive-scope-component
                                  (string-downcase which))))))
    (make-child-scope participant scope kind)))

(defmethod make-child-scope ((participant t)
                             (which       scope)
                             (kind        t))
  (merge-scopes which (participant-scope participant)))

(defmethod make-child-scope ((participant t)
                             (which       puri:uri)
                             (kind        t))
  (make-child-scope participant (uri->scope-and-options which) kind))

(defmethod make-child-initargs ((participant t)
                                (which       t)
                                (kind        t)
                                &rest args &key)
  args)

(defmethod make-child-initargs ((participant rsb::error-hook-mixin)
                                (which       t)
                                (kind        t)
                                &key)
  ;; Connect the error-hook of the created child participant to the
  ;; error-hook of PARTICIPANT (the parent).
  (let ((error-hook (participant-error-hook participant)))
    (list* :error-policy (lambda (condition)
                           (hooks:run-hook error-hook condition))
           (when (next-method-p)
             (call-next-method)))))

(defmethod make-child-participant ((participant t)
                                   (which       t)
                                   (kind        t)
                                   &rest args &key)
  (let ((scope    (make-child-scope participant which kind))
        (initargs (apply #'make-child-initargs participant which kind args)))
    (apply #'make-participant kind scope :parent participant initargs)))
