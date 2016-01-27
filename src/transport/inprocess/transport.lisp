;;;; transport.lisp --- inprocess transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; `inprocess-transport' transport class

  (defclass inprocess-transport (transport)
    ((scope-sinks      :reader   transport-scope-sinks
                       :initform (rsb.ep:make-sink-scope-trie)
                       :documentation
                       "Association of scopes to event sinks interested in
                        the respective scopes.")
     (machine-instance :initarg  :machine-instance
                       :type     string
                       :reader   transport-machine-instance
                       :writer   (setf transport-%machine-instance)
                       :documentation
                       "Stores the machine instance string that
                        connectors created by this transport should
                        use in their URLs.")
     (process-id       :initarg  :process-id
                       :type     non-negative-integer
                       :reader   transport-process-id
                       :writer   (setf transport-%process-id)
                       :documentation
                       "Stores the process id that connectors created
                        by this transport should use in their URLs."))
    (:default-initargs
     :machine-instance (machine-instance)
     :process-id       (sb-posix:getpid)))

;;; Register transport

  (register-transport
   :inprocess
   :transport-class 'inprocess-transport
   :schemas         :inprocess
   :wire-type       t ; The Lisp process is the medium, so t (any Lisp
                      ; object) should be a reasonable wire-type
   :remote?         nil
   :documentation
   "Transport for communication within one process.

    RSB participants within one process can exchange events
    efficiently without (de)serialization and a lock-free routing data
    structure.")

) ; eval-when

;;; Global transport instance

(defun update-machine-instance-and-process-id ()
  (let ((transport (service-provider:find-provider 'transport :inprocess)))
    (setf (transport-%machine-instance transport) (machine-instance)
          (transport-%process-id transport)       (sb-posix:getpid))))

#+sbcl (pushnew 'update-machine-instance-and-process-id
                sb-ext:*init-hooks*)
#-sbcl (pushnew 'update-machine-instance-and-process-id
                uiop:*image-restore-hook*)
