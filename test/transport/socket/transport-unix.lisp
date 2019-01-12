;;;; transport-unix.lisp --- Unit tests for UNIX transport and bus.
;;;;
;;;; Copyright (C) 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

(def-suite transport-socket-unix-root
    :in transport-socket-root
    :description
    "Unit tests for the UNIX transport and bus.")
(in-suite transport-socket-unix-root)

(test client/smoke
  "Test creating `bus-client' instances and attaching and detaching
   connectors to them.

   Try multiple connectors of different classes which also causes the
   test case to be repeated without a fresh port. This helps ensuring
   proper cleanup."

  (mapc
   (lambda+ ((connector-class schema address))
     (let ((transport   (service-provider:find-provider
                         'rsb.transport:transport schema))
           (connector-1 (make-socket-connector connector-class schema address))
           (connector-2 (make-socket-connector connector-class schema address)))

       ;; There is no server yet, so this has to signal an error.
       (signals usocket:connection-refused-error ; TODO(jmoringe): keep this condition type?
         (transport-ensure-bus transport :client! connector-1 address))

       ;; Create a bus server.
       (with-participant (nil :reader (make-socket-url :unix-socket t address)
                              :converters '((t . :fundamental-null)))
         ;; We should be able to create a bus clients now. We create
         ;; two connectors and request a bus client for each of
         ;; them. The first request should cause the bus client to be
         ;; created, while the second should just return the existing
         ;; bus client.
         (let ((bus-1 (transport-ensure-bus
                       transport :client! connector-1 address))
               (bus-2 (transport-ensure-bus
                       transport :client! connector-2 address)))
           (check-buses-and-connectors
            (list bus-1 bus-2) (list connector-1 connector-2) t)))))

   (mappend (lambda+ ((class schema address))
              (make-list 10 :initial-element `(,class ,schema ,address)))
            (let ((name (funcall (gen-string :length (constantly 8)))))
              `((unix-in-pull-connector :unix-socket (:name ,name))
                (unix-in-push-connector :unix-socket (:name ,name))
                (unix-out-connector     :unix-socket (:name ,name)))))))

(test server/smoke
  "Test creating `bus-server' instances and attaching and detaching
   connectors to them.

   Try multiple connectors of different classes which also causes the
   test case to be repeated without a fresh port. This helps ensuring
   proper cleanup."

  (mapc
   (lambda+ ((connector-class schema address))
     (let ((transport   (service-provider:find-provider
                         'rsb.transport:transport :unix-socket))
           (connector-1 (make-socket-connector
                         connector-class schema address :server? t))
           (connector-2 (make-socket-connector
                         connector-class schema address :server? t)))

       ;; Create two connectors and request a bus server for each of
       ;; them. The first request should cause the bus server to be
       ;; created, while the second should just return the existing bus
       ;; server. Creating a bus server should succeed unless the port
       ;; is in use.
       (let ((bus-1 (transport-ensure-bus
                     transport :server! connector-1 address))
             (bus-2 (transport-ensure-bus
                     transport :server! connector-2 address)))
         (check-buses-and-connectors
          (list bus-1 bus-2) (list connector-1 connector-2)))))

   (mappend (lambda+ ((class schema address))
              (make-list 10 :initial-element `(,class ,schema ,address)))
            (let ((name (funcall (gen-string :length (constantly 8)))))
              `((unix-in-pull-connector :unix-socket (:name ,name))
                (unix-in-push-connector :unix-socket (:name ,name))
                (unix-out-connector     :unix-socket (:name ,name)))))))
