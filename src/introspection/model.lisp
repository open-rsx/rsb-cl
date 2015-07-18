;;;; model.lisp --- Model classes used by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Functions related to `process-info'

(defun current-process-info ()
  "Make and return a `process-info' instance describing the current
   process."
  (with-platform-information-fallback-values
    (let+ (((program-name &rest arguments)
            (current-program-name-and-commandline-arguments)))
      (make-instance
       'process-info
       :process-id            (current-process-id)
       :program-name          (pathname-name
                               (parse-namestring program-name))
       :commandline-arguments arguments
       :start-time            (current-process-start-time)
       :executing-user        (current-user)
       :rsb-version           (cl-rsb-system:version/string :commit? t)
       :display-name          (option-value '(:introspection :displayname))))))

;;; Functions related to `host-info'

(defun current-host-info ()
  "Return a `host-info' instance describing the local host."
  (with-platform-information-fallback-values
    (make-instance 'host-info
                   :id               (current-host-id)
                   :hostname         (current-hostname)
                   :machine-type     (current-machine-type)
                   :machine-version  (current-machine-version)
                   :software-type    (current-software-type)
                   :software-version (current-software-version))))

;;; `hello' message

(defclass hello ()
  ((participant :initarg  :participant
                :type     participant-info
                :reader   hello-participant
                :documentation
                "Stores information about the created participant.")
   (process     :initarg  :process
                :type     process-info
                :reader   hello-process
                :documentation
                "Stores information about the local process.")
   (host        :initarg  :host
                :type     host-info
                :reader   hello-host
                :documentation
                "Stores information about the local host."))
  (:default-initargs
   :participant (missing-required-initarg 'hello :participant)
   :process     (missing-required-initarg 'hello :process)
   :host        (missing-required-initarg 'hello :host))
  (:documentation
   "Instances of this message are published when participants are
    created in the local process and may be received when participants
    are created in remote processes."))

(defmethod print-object ((object hello) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~/print-items:format-print-items/"
            (mapcar
             (lambda+ ((&whole entry key value &optional &ign &ign))
               (case key
                 (:id
                  (list key value " ~/rsb::print-id/" '((:after :transports))))
                 (t
                  entry)))
             (print-items:print-items (hello-participant object))))))

;;; `bye' message

(defclass bye (uuid-mixin)
  ((rsb::id :initarg  :id
            :reader   bye-id
            :documentation
            "Stores the unique id of the detached participant."))
  (:default-initargs
   :id (missing-required-initarg 'bye :id))
  (:documentation
   "Instances of this message are published when participants are
    detached in the local process and may be received when
    participants are destroyed in remote processes."))

(defmethod print-object ((object bye) stream)
  (print-unreadable-id-object (object stream)))
