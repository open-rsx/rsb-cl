;;;; conversion.lisp --- Converter for introspection messages.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

(macrolet
    ((define-introspection-converter
         ((model-class notification-class wire-schema)
          &key wire->domain domain->wire)
       `(progn
          (defmethod rsb.converter:wire->domain?
              ((converter   (eql :introspection))
               (wire-data   simple-array)
               (wire-schema (eql ,wire-schema)))
            (values :introspection ',model-class))

          (defmethod rsb.converter:domain->wire?
              ((converter     (eql :introspection))
               (domain-object ,model-class))
            (values :introspection 'nibbles:octet-vector ,wire-schema))

          (defmethod rsb.converter:wire->domain
              ((converter   (eql :introspection))
               (wire-data   simple-array)
               (wire-schema (eql ,wire-schema)))
            (let ((notification (pb:unpack wire-data ',notification-class)))
              ,wire->domain))

          (defmethod rsb.converter:domain->wire
              ((converter     (eql :introspection))
               (domain-object ,model-class))
            (values (pb:pack* ,domain->wire) ,wire-schema)))))

  (define-introspection-converter
      (hello
       rsb.protocol.introspection:hello
       :|.rsb.protocol.introspection.Hello|)

    :wire->domain
    (let+ (((&accessors-r/o
             (participant-id rsb.protocol.introspection:hello-id)
             (kind           rsb.protocol.introspection:hello-kind)
             (type           rsb.protocol.introspection:hello-type)
             (transports     rsb.protocol.introspection:hello-transport)
             (scope          rsb.protocol.introspection:hello-scope)
             (process        rsb.protocol.introspection:hello-process)
             (host           rsb.protocol.introspection:hello-host))
            notification)
           ((&accessors-r/o
             (process-id            rsb.protocol.operatingsystem:process-id)
             (program-name          rsb.protocol.operatingsystem:process-program-name)
             (commandline-arguments rsb.protocol.operatingsystem:process-commandline-arguments)
             (start-time            rsb.protocol.operatingsystem:process-start-time)
             (executing-user        rsb.protocol.operatingsystem:process-executing-user)
             (rsb-version           rsb.protocol.operatingsystem:process-rsb-version))
            process)
           ((&accessors-r/o
             (host-id          rsb.protocol.operatingsystem:host-id)
             (hostname         rsb.protocol.operatingsystem:host-hostname)
             (machine-type     rsb.protocol.operatingsystem:host-machine-type)
             (machine-version  rsb.protocol.operatingsystem:host-machine-version)
             (software-type    rsb.protocol.operatingsystem:host-software-type)
             (software-version rsb.protocol.operatingsystem:host-software-version))
            host))
      (make-instance
       'hello
       :participant (make-instance
                     'remote-participant-info
                     :kind       (make-keyword (string-upcase kind))
                     :id         (uuid:byte-array-to-uuid
                                  participant-id)
                     :parent-id  (when (and (rsb.protocol.introspection:hello-parent?
                                             notification)
                                            (not (emptyp (rsb.protocol.introspection:hello-parent
                                                          notification))))
                                   (uuid:byte-array-to-uuid
                                    (rsb.protocol.introspection:hello-parent
                                     notification)))
                     :scope      (make-scope scope)
                     :type       (if (emptyp type) ; TODO
                                     t
                                     type
                                     #+no (let ((*package* (find-package :keyword)))
                                            (read-from-string type)))
                     :transports (map 'list #'puri:uri transports))
       :process     (make-instance
                     'process-info
                     :process-id            (parse-integer process-id)
                     :program-name          program-name
                     :commandline-arguments (coerce commandline-arguments 'list)
                     :start-time            (unix-microseconds->timestamp
                                             start-time)
                     :executing-user        (unless (emptyp executing-user)
                                              executing-user)
                     :rsb-version           (unless (emptyp rsb-version)
                                              rsb-version))
       :host        (make-instance
                     'host-info
                     :id               host-id
                     :hostname         hostname
                     :machine-type     (unless (emptyp machine-type)
                                         machine-type)
                     :machine-version  (unless (emptyp machine-version)
                                         machine-version)
                     :software-type    (unless (emptyp software-type)
                                         software-type)
                     :software-version (unless (emptyp software-version)
                                         software-version))))

    :domain->wire
    (let+ (((&structure-r/o hello- participant process host) domain-object)
           ((&structure-r/o
             participant-info- kind id scope type transports parent-id)
            participant)
           ((&structure-r/o
             process-info-
             process-id program-name commandline-arguments
             start-time executing-user rsb-version)
            process)
           ((&structure-r/o
             host-info- (host-id id) hostname
             machine-type machine-version software-type software-version)
            host))
      (apply #'make-instance 'rsb.protocol.introspection:hello
             :kind      (string-downcase kind)
             :id        (uuid:uuid-to-byte-array id)
             :scope     (scope-string scope)
             :type      (prin1-to-string type)
             :transport (map 'vector #'princ-to-string transports)
             :process   (make-instance
                         'rsb.protocol.operatingsystem:process
                         :id                    (prin1-to-string process-id)
                         :program-name          program-name
                         :commandline-arguments (coerce commandline-arguments
                                                        '(vector string))
                         :start-time            (timestamp->unix-microseconds
                                                 start-time)
                         :executing-user        executing-user
                         :rsb-version           rsb-version)
             :host      (make-instance
                         'rsb.protocol.operatingsystem:host
                         :id               host-id
                         :hostname         hostname
                         :machine-type     machine-type
                         :machine-version  machine-version
                         :software-type    software-type
                         :software-version software-version)
             (when parent-id
               (list :parent (uuid:uuid-to-byte-array parent-id))))))

  (define-introspection-converter
      (bye
       rsb.protocol.introspection:bye
       :|.rsb.protocol.introspection.Bye|)

    :wire->domain
    (make-instance 'bye
                   :id (uuid:byte-array-to-uuid
                        (rsb.protocol.introspection:bye-id notification)))

    :domain->wire
    (make-instance
     'rsb.protocol.introspection:bye
     :id (uuid:uuid-to-byte-array (bye-id domain-object)))))

;;; Utility functions
;;; TODO these will be part of generic serialization/protocol helper functions

(defun timestamp->unix-microseconds (timestamp)
  "Convert the `local-time:timestamp' instance TIMESTAMP into an
   integer which counts the number of microseconds since UNIX epoch."
  (+ (* 1000000 (local-time:timestamp-to-unix timestamp))
     (* 1       (local-time:timestamp-microsecond timestamp))))

(defun unix-microseconds->timestamp (unix-microseconds)
  "Convert UNIX-MICROSECONDS to an instance of
   `local-time:timestamp'."
  (let+ (((&values unix-seconds microseconds)
          (floor unix-microseconds 1000000)))
    (local-time:unix-to-timestamp
     unix-seconds :nsec (* 1000 microseconds))))
