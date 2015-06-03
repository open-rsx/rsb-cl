;;;; mixins.lisp --- Mixin classes for communication pattern implementations.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns)

;;; `composite-participant-mixin'

(defclass composite-participant-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into participant classes that
    contain child participants.

    It provides appropriate disposal and printing functionality but no
    storage for the child participants."))

(defmethod print-items:print-items append ((object composite-participant-mixin))
  `((:num-children ,(length (participant-children object)) " (~D)")))

(defmethod detach ((participant composite-participant-mixin))
  (mapc (lambda (child)
          (with-restart-and-timeout (10)
            (detach child)))
        (participant-children participant))
  (when (next-method-p)
    (call-next-method)))

(defmethod transport-specific-urls ((component composite-participant-mixin))
  (let ((uris (mapcar (curry #'puri:merge-uris (relative-url component))
                      (mappend #'transport-specific-urls
                               (participant-children component)))))
    (remove-duplicates uris :test #'puri:uri=)))

;;; `child-container-mixin'

(defclass child-container-mixin ()
  ((children :type     hash-table
             :reader   participant-%children
             :initform (make-hash-table :test #'equal)
             :documentation
             "Stores a `hash-table' mapping \"child keys\" of the
              form

                (KIND . WHICH)

              to child `participant' instances. KIND is a participant
              kind and WHICH (with KIND) uniquely identifies the child
              within the containing composite participant."))
  (:documentation
   "This class is intended to be mixed into participant classes that
    store child participants which can be uniquely identified by a
    kind and a name."))

(defmethod participant-children ((participant child-container-mixin))
  (hash-table-values (participant-%children participant)))

(defmethod participant-child ((participant child-container-mixin)
                              (which       t)
                              (kind        t)
                              &key
                              if-does-not-exist
                              if-exists
                              detach?)
  (declare (ignore if-does-not-exist if-exists detach?))
  (let ((key (cons kind which)))
    (values (gethash key (participant-%children participant)))))

(defmethod (setf participant-child) ((new-value   t)
                                     (participant child-container-mixin)
                                     (which       t)
                                     (kind        t)
                                     &key
                                     if-does-not-exist
                                     if-exists
                                     detach?)
  (declare (ignore if-does-not-exist if-exists detach?))
  (let ((key (cons kind which)))
    (setf (gethash key (participant-%children participant)) new-value)))

(defmethod (setf participant-child) ((new-value   (eql nil))
                                     (participant child-container-mixin)
                                     (which       t)
                                     (kind        t)
                                     &key
                                     if-does-not-exist
                                     if-exists
                                     (detach?          t))
  (declare (ignore if-does-not-exist if-exists))
  (let+ (((&structure-r/o participant- %children) participant)
         (key      (cons kind which))
         (existing (gethash key %children)))
    (when (and existing detach?)
      (with-restart-and-timeout (10)
        (detach existing)))
    (remhash key %children))
  new-value)

;;; `configuration-inheritance-mixin'

(defclass configuration-inheritance-mixin ()
  ((transport-options     :initarg  :transport-options
                          :type     list
                          :reader   participant-transport-options
                          :documentation
                          "Stores the transport options that should be
                           passed to child participants when they are
                           created.

                           The stored options have already been merged
                           with defaults and thus make created child
                           participants ignore any special binding of
                           `*configuration*'.")
   (converter-options     :initarg  :converter-options
                          :reader   participant-converter-options
                          :documentation
                          "Stores the converter options that should be
                           passed to child participants when they are
                           created.

                           TODO this is a temporary workaround until
                           participant-configuration is ready.")
   (transform-option      :initarg  :transform-option
                          :reader   participant-transform-option
                          :initform nil
                          :documentation
                          "Stores the transform which should be applied to
                           processed events.

                           TODO this is a temporary workaround until
                           participant-configuration is ready.")
   (introspection?-option :initarg  :introspection?-option
                          :type     boolean
                          :reader   participant-introspection?-option
                          :initform t
                          :documentation
                          "Stores a Boolean indicating whether
                           introspection has enabled for the
                           participant.

                           TODO this is a temporary workaround until
                           participant-configuration is ready."))
  (:documentation
   "This class is intended to be mixed into participant classes that
    relay configuration options to their child participant."))

(defmethod shared-initialize :before
    ((instance   configuration-inheritance-mixin)
     (slot-names t)
     &key
     (transform nil transform-supplied?))
  ;; We store transform-options for passing them to methods but do not
  ;; accept transform for ourselves.
  (when transform-supplied?
    (incompatible-initargs 'configuration-inheritance-mixin
                           :transform transform)))

(defmethod make-participant-using-class
    ((class     class)
     (prototype configuration-inheritance-mixin)
     (scope     scope)
     &rest args &key
     (transports     '())
     (converters     (default-converters))
     transform
     (introspection? (when (member (option-value '(:introspection :enabled)) '(t "1")
                                   :test #'equal)
                       t)))
  ;; "Freeze" transport options by merging with defaults, thereby
  ;; disabling further inheritance from special bindings of
  ;; `*configuration*' during child participant creation (may even
  ;; happen in a different thread with different special bindings).
  ;;
  ;; Also check that TRANSPORTS will select at least one transport.
  (let ((transport-options (rsb::merge-transport-options
                            transports (transport-options))))
    (unless (rsb::effective-transport-options transport-options)
      (error 'no-transports-error
             :kind  (participant-kind prototype)
             :scope scope))

    (apply #'call-next-method class prototype scope
           :transport-options     transport-options
           :converter-options     converters
           :transform-option      transform
           :introspection?-option introspection?
           (remove-from-plist args :converters :transports :transform))))

(defmethod make-child-initargs ((participant configuration-inheritance-mixin)
                                (which       t)
                                (kind        t)
                                &key)
  (let+ (((&structure-r/o
           participant-
           transport-options converter-options transform-option
           introspection?-option)
          participant))
    (list* :transports     transport-options
           :converters     converter-options
           :transform      transform-option
           :introspection? introspection?-option
           (when (next-method-p)
             (call-next-method)))))

;;; `lazy-child-making-mixin'

(defclass lazy-child-making-mixin ()
  ()
  (:documentation
   "This mixin class adds the ability to create child participants
    when they are first requested."))

(defmethod participant-child ((participant lazy-child-making-mixin)
                              (which       t)
                              (kind        t)
                              &key
                              (if-does-not-exist :create)
                              if-exists
                              detach?)
  (declare (ignore if-exists detach?))
  (case if-does-not-exist
    (:create
     (or (call-next-method participant which kind :if-does-not-exist nil)
         ;; If necessary, create the requested child. Make sure to
         ;; detach it if it cannot be installed.
         (let ((child (make-child-participant participant which kind)))
           (unwind-protect-case ()
               (setf (participant-child participant which kind) child)
             (:abort
              (detach/ignore-errors child))))))
    (t
     (call-next-method))))
