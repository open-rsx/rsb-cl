;;;; classes.lisp --- Classes of the model module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model)

;;; `participant-info'

(defclass participant-info (rsb:uuid-mixin
                            rsb:scope-mixin
                            print-items:print-items-mixin)
  ((kind       :initarg  :kind
               :type     keyword
               :reader   participant-info-kind
               :documentation
               "Stores the kind of the participant. Usually similar to
                the class, for example :reader, :listener, :informer.")
   (rsb::id    :initarg  :id
               :reader   participant-info-id)
   (parent-id  :initarg  :parent-id
               :type     (or null uuid:uuid)
               :reader   participant-info-parent-id
               :initform nil
               :documentation
               "If not nil, stores the unique id of the composite
                participant containing the participant.")
   (scope      :reader   participant-info-scope)
   (type       :initarg  :type
               :reader   participant-info-type
               :documentation
               "Stores some indication of a type of the participant.

                TODO Currently underspecified. Likely to change.")
   (transports :initarg  :transports
               :type     list #| of puri:uri |#
               :reader   participant-info-transports
               :initform '()
               :documentation
               "Stores information about the transports used by the
                participant.

                TODO Currently underspecified. Likely to change."))
  (:default-initargs
   :kind (missing-required-initarg 'participant-info :kind)
   :id   (missing-required-initarg 'participant-info :id)
   :type (missing-required-initarg 'participant-info :type))
  (:documentation
   "Instances of this class store information about participants."))

(defmethod print-items:print-items append ((object participant-info))
  (let+ (((&structure-r/o participant-info- kind scope transports) object))
    `((:kind       ,kind                 "~A"      ((:before :id)
                                                    (:before :scope)))
      (:scope      ,(scope-string scope) " ~A"     ((:before :transports))) ; TODO remove when the mixins get print-items methods
      (:transports ,(length transports)  " (T ~D)"))))

(defmethod print-object ((object participant-info) stream)
  (rsb::print-unreadable-id-object (object stream :type nil)
    (format stream "~/print-items:format-print-items/"
            (remove :id (print-items:print-items object) :key #'first))))

;;; `remote-participant-info'

(defclass remote-participant-info (participant-info)
  ()
  (:documentation
   "Instances of this class information about participants in remote
    processes extending the information in `participant-info'."))

;;; `process-info'

(defclass process-info (print-items:print-items-mixin)
  ((process-id            :initarg  :process-id
                          :type     non-negative-integer
                          :reader   process-info-process-id
                          :documentation
                          "Stores a unique numeric identifier of the
                           process.")
   (program-name          :initarg  :program-name
                          :type     string
                          :reader   process-info-program-name
                          :documentation
                          "Stores the name of the program executed in
                           the process.")
   (commandline-arguments :initarg  :commandline-arguments
                          :type     list #|of string|#
                          :reader   process-info-commandline-arguments
                          :initform '()
                          :documentation
                          "Stores the commandline arguments with which
                           the process has been started.")
   (start-time            :initarg  :start-time
                          :type     local-time:timestamp
                          :reader   process-info-start-time
                          :initform (local-time:now)
                          :documentation
                          "Stores (an approximation of) the start time
                           of the process.")
   (executing-user        :initarg  :executing-user
                          :type     (or null string)
                          :reader   process-info-executing-user
                          :initform nil
                          :documentation
                          "Stores the login- or account-name of the
                           user executing the process.")
   (rsb-version           :initarg  :rsb-version
                          :type     (or null string)
                          :reader   process-info-rsb-version
                          :initform nil
                          :documentation
                          "Sores the version of the RSB implementation
                           used in the process.

                           The version string is of the form

                             MAJOR.MINOR.REVISION[-COMMIT]")
   (display-name          :initarg  :display-name
                          :type     (or null string)
                          :reader   process-info-display-name
                          :initform nil
                          :documentation
                          "If not null, stores a user-defined name for
                           the process."))
  (:default-initargs
   :program-name (missing-required-initarg 'process-info :program-name)
   :process-id   (missing-required-initarg 'process-info :process-id))
  (:documentation
   "Instances of this class store information of about processes."))

(defmethod print-items:print-items append ((object process-info))
  (let+ (((&structure-r/o process-info- program-name process-id display-name)
          object))
    `((:program-name ,(list display-name program-name)
                     ,"~{~:[~A~:;~:*~A(~A)~]~}"
                     ((:before :process-id)))
      (:process-id   ,process-id   "[~D]"))))

;;; `remote-process-info'

(defclass remote-process-info (process-info
                               timing-info-mixin
                               most-recent-activity-mixin)
  ((state      :initarg  :state
               :type     process-state
               :accessor process-info-state
               :initform :unknown
               :documentation
               "Stores the inferred state of the remote process.")
   (transports :initarg  :transports
               :type     list #| of puri:uri |#
               :accessor process-info-transports
               :documentation
               "Stores a list of transports URIs describing transports
                that can be used to contact the process."))
  (:default-initargs
   :transports (missing-required-initarg 'remote-process-info :transports))
  (:documentation
   "Instances of this class store information about remote processes
    extending the information in `process-info'."))

(defmethod print-items:print-items append ((object remote-process-info))
  (let+ (((&structure-r/o process-info- state) object))
    `((:process-state ,state " ~A" ((:after :process-id))))))

;;; `host-info'

(defclass host-info (print-items:print-items-mixin)
  ((id               :initarg  :id
                     :type     string
                     :reader   host-info-id
                     :documentation
                     "Stores a string (hopefully) uniquely identifying
                      the host.")
   (hostname         :initarg  :hostname
                     :type     string
                     :reader   host-info-hostname
                     :documentation
                     "Stores the name of the host.")
   (machine-type     :initarg  :machine-type
                     :type     (or null string)
                     :reader   host-info-machine-type
                     :initform nil
                     :documentation
                     "Stores the type of the machine, usually CPU
                      architecture.")
   (machine-version  :initarg  :machine-version
                     :type     (or null string)
                     :reader   host-info-machine-version
                     :initform nil
                     :documentation
                     "Stores the version of the machine within its
                      type, usually the CPU identification string.")
   (software-type    :initarg  :software-type
                     :type     (or null string)
                     :reader   host-info-software-type
                     :initform nil
                     :documentation
                     "Stores the type of the operating system running
                      on the host, usually the kernel name.")
   (software-version :initarg  :software-version
                     :type     (or null string)
                     :reader   host-info-software-version
                     :initform nil
                     :documentation
                     "Stores the version of the operating system
                      within its type, usually the kernel version
                      string."))
  (:default-initargs
   :id       (missing-required-initarg 'host-info :id)
   :hostname (missing-required-initarg 'host-info :hostname))
  (:documentation
   "Instances of this class store information about a host."))

(defmethod print-items:print-items append ((object host-info))
  (let+ (((&structure-r/o host-info- hostname machine-type software-type)
          object))
    `((:hostname      ,hostname)
      (:machine-type  ,machine-type  " ~:[?~:;~:*~A~]" ((:after :hostname)))
      (:software-type ,software-type " ~:[?~:;~:*~A~]" ((:after :machine-type))))))

;;; `remote-host-info'

(defclass remote-host-info (host-info
                            timing-info-mixin
                            most-recent-activity-mixin)
  ((state :initarg  :state
          :type     host-state
          :accessor host-info-state
          :initform :unknown
          :documentation
          "The stored state indicates whether the remote host is
           assumed to be up, down or in an unknown state."))
  (:documentation
   "Instances of this class store information about remote hosts
    extending the information in `host-info'."))

(defmethod print-items:print-items append ((object remote-host-info))
  (let+ (((&accessors-r/o (state        host-info-state)
                          (clock-offset info-clock-offset))
          object))
    `((:host-state ,state " ~A" ((:after :hostname)
                                 (:after :machine-type)
                                 (:after :software-type)))
      ,@(when (and clock-offset (> (abs clock-offset) 0.001))
          `((:clock-offset ,clock-offset " ~,3@F s" ((:after :host-state))))))))

;;; `node' and `{participant,process,host}-node' subclasses

(defclass node ()
  ()
  (:documentation
   "Superclass for classes, instances of which are nodes in the model
    tree."))

(defmethod print-items:print-items append ((object node))
  `((:num-children ,(length (node-children object)) " (C ~D)")))

(defclass participant-node (node)
  ()
  (:documentation
   "Instances represent participants (via `participant-info') and
    their relations within the model tree."))

(defmethod print-items:print-items append ((object participant-node))
  `((:num-children ,(length (node-children object)) " (C ~D)"
                   ((:after :transports) (:before :id)))))

(defmethod print-object ((object participant-node) stream)
  (print-unreadable-object (object stream :type t)
    (print-items:format-print-items stream (print-items:print-items object))))

(defclass process-node (node)
  ()
  (:documentation
   "Instances represent processes (via `process-info') and their
    relations within the model tree."))

(defmethod print-items:print-items append ((object process-node))
  `((:num-children ,(length (node-children object)) " (C ~D)"
                   ((:after :process-id) (:before :process-state)))))

(defclass host-node (node)
  ()
  (:documentation
   "Instances represent hosts (via `host-info') and their relations
    within the model tree."))

(defmethod print-items:print-items append ((object host-node))
  `((:num-children ,(length (node-children object)) " (C ~D)"
                   ((:after :software-type) (:before :host-state)))))

;;; Node-related convenience classes

(defclass basic-participant-node (info-mixin
                                  participant-node
                                  parented-mixin
                                  child-container-mixin
                                  print-items:print-items-mixin)
  ((info   :type     participant-info)
   (parent :type     (or null participant-node process-node))))

(defclass basic-process-node (info-mixin
                              process-node
                              parented-mixin
                              child-container-mixin
                              print-items:print-items-mixin)
  ((info   :type     process-info)
   (parent :type     (or null host-node))))

(defclass basic-host-node (info-mixin
                           host-node
                           child-container-mixin
                           print-items:print-items-mixin)
  ((info   :type     host-info)))
