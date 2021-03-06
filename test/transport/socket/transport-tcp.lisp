;;;; transport-tcp.lisp --- Unit tests for TCP transport and bus.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

(deftestsuite transport-socket-tcp-root (transport-socket-root)
  ()
  (:documentation
   "Unit tests for the TCP transport and bus."))

(addtest (transport-socket-tcp-root
          :documentation
          "Test supplying incompatible options for a single
           connection.")
  incompatible-options

  ;; Has to signal an error since the incompatible options are
  ;; supplied for a single host-port combination.
  (ensure-condition participant-creation-error
    (with-participants
        ((nil :reader (make-socket-url :tcp-socket t nil))
         (nil :reader (make-socket-url :tcp-socket nil '("tcpnodelay" "0")))
         (nil :reader (make-socket-url :tcp-socket nil nil)))))) ; body is not important

(addtest (transport-socket-tcp-root
          :documentation
          "Test creating `bus-client' instances and attaching and
           detaching connectors to them.

           Try multiple connectors of different classes which also
           causes the test case to be repeated without a fresh
           port. This helps ensuring proper cleanup.")
  client/smoke

  (ensure-cases (connector-class schema address)
      (mappend (lambda+ ((class schema address))
                 (make-list 10 :initial-element `(,class ,schema ,address)))
               `((tcp-in-pull-connector :tcp-socket
                                        (:host "localhost" :port ,*next-port*))
                 (tcp-in-push-connector :tcp-socket
                                        (:host "localhost" :port ,*next-port*))
                 (tcp-out-connector     :tcp-socket
                                        (:host "localhost" :port ,*next-port*))))

    (let ((transport   (service-provider:find-provider
                        'rsb.transport:transport schema))
          (connector-1 (make-socket-connector connector-class schema address))
          (connector-2 (make-socket-connector connector-class schema address)))

      ;; There is no server yet, so this has to signal an error.
      (ensure-condition 'usocket:connection-refused-error ; TODO(jmoringe): keep this condition type?
        (transport-ensure-bus transport :client! connector-1 address))

      ;; Create a bus server.
      (with-participant (nil :reader (make-socket-url :tcp-socket t nil)
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
           (list bus-1 bus-2) (list connector-1 connector-2) t))))))

(addtest (transport-socket-tcp-root
          :documentation
          "Test creating `bus-server' instances and attaching and
           detaching connectors to them.

           Try multiple connectors of different classes which also
           causes the test case to be repeated without a fresh
           port. This helps ensuring proper cleanup.")  server/smoke

  (ensure-cases (connector-class schema address)
      (mappend (lambda+ ((class schema address))
                 (make-list 10 :initial-element `(,class ,schema ,address)))
               `((tcp-in-pull-connector :tcp-socket
                                        (:host "localhost" :port ,*next-port*))
                 (tcp-in-push-connector :tcp-socket
                                        (:host "localhost" :port ,*next-port*))
                 (tcp-out-connector     :tcp-socket
                                        (:host "localhost" :port ,*next-port*))))

    (let ((transport   (service-provider:find-provider
                        'rsb.transport:transport :tcp-socket))
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
         (list bus-1 bus-2) (list connector-1 connector-2))))))

(addtest (transport-socket-tcp-root
          :documentation
          "Test automatically assigned ports and the portfile option
           using the -[2] syntax.")
  server/automatic-port.1

  (let* ((host        "localhost")
         (port        0)
         (address     (list :host host :port port))
         (transport   (service-provider:find-provider
                       'rsb.transport:transport :tcp-socket))
         (connector-1 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server? t :portfile "-"))
         (connector-2 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server? t :portfile "-2")))

    ;; Create two connectors and request a bus server with automatic
    ;; port assignment for each of them. The first request should
    ;; cause the bus server to be created, while the second should
    ;; just return the existing bus server. Creating a bus server
    ;; should succeed unless the port is in use.
    (let+ (((&flet ensure-bus (connector stream-name)
              (let+ ((bus)
                     (port (with-output-to-string (stream)
                             (progv (list stream-name) (list stream)
                               (setf bus (transport-ensure-bus
                                          transport :server! connector
                                          address))))))
                (values bus port))))
           ((&values bus-1 port-1)
            (ensure-bus connector-1 '*standard-output*))
           ((&values bus-2 port-2)
            (ensure-bus connector-2 '*error-output*)))
      ;; Make sure that both connectors got the same port via their
      ;; respective "portfile" outputs.
      (ensure-same port-1 port-2 :test #'string=)

      (check-buses-and-connectors
       (list bus-1 bus-2) (list connector-1 connector-2)))))

(addtest (transport-socket-tcp-root
          :documentation
          "Test automatically assigned ports and the portfile option
           with multiple/mixed portfile options.")
  server/automatic-port.2

  (let* ((host        "localhost")
         (port        0)
         (address     (list :host host :port port))
         (transport   (service-provider:find-provider
                       'rsb.transport:transport :tcp-socket))
         (connector-1 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server? t))
         (connector-2 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server? t :portfile "-"))
         (connector-3 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server? t :portfile "-")))

    ;; Create three connectors and request a bus server with automatic
    ;; port assignment for each of them. The first connector does not
    ;; specify a port-file while the second and third do. The correct
    ;; behavior is writing the port-file once for the second connector
    ;; (i.e. not a second time for the third connector).
    (let+ (((&flet ensure-bus (connector)
              (let+ ((bus)
                     (output (with-output-to-string (*standard-output*)
                               (setf bus (transport-ensure-bus
                                          transport :server! connector
                                          address)))))
                (values bus output))))
           ((&values bus-1 output-1) (ensure-bus connector-1))
           ((&values bus-2 output-2) (ensure-bus connector-2))
           ((&values bus-3 output-3) (ensure-bus connector-3)))
      ;; Make sure that only the second connector caused the port-file
      ;; to be written.
      (ensure-same      output-1 "" :test #'string=)
      (ensure-different output-2 "" :test #'string=)
      (ensure-same      output-3 "" :test #'string=)

      (check-buses-and-connectors
       (list bus-1 bus-2 bus-3) (list connector-1 connector-2 connector-3)))))

#+(and sbcl (not win32))
(addtest (transport-socket-tcp-root
          :documentation
          "Test automatically assigned ports and the portfile option
           with custom file descriptors.")
  server/automatic-port.portfile-fd

  (let+ (((&values read-fd write-fd) (sb-posix:pipe))
         (portfile  (format nil "-~D" write-fd))
         (host      "localhost")
         (port      0)
         (address   (list :host host :port port))
         (transport (service-provider:find-provider
                     'rsb.transport:transport :tcp-socket))
         (connector (make-socket-connector
                     'tcp-connector :tcp-socket address
                     :server? t :portfile portfile))
         (bus       (transport-ensure-bus
                     transport :server! connector address))
         (output    (with-open-stream (stream (sb-sys:make-fd-stream
                                               read-fd :input t))
                      (read-line stream))))
    (ensure (typep (parse-integer output) '(unsigned-byte 16)))))

(defvar *port-promise*)

(defun note-port (port)
  (lparallel:fulfill *port-promise* port))

(addtest (transport-socket-tcp-root
          :documentation
          "Test automatically assigned ports and the portfile
           option using the call:NAME syntax.")
  server/automatic-port.call

  (let* ((host        "localhost")
         (port        0)
         (address     (list :host host :port port))
         (transport   (service-provider:find-provider
                       'rsb.transport:transport :tcp-socket))
         (connector-1 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server?  t
                       :portfile "call:rsb.transport.socket.test::note-port"))
         (connector-2 (make-socket-connector
                       'tcp-connector :tcp-socket address
                       :server?  t
                       :portfile "call:rsb.transport.socket.test::note-port")))

    ;; Create two connectors and request a bus server with automatic
    ;; port assignment for each of them. The first request should
    ;; cause the bus server to be created, while the second should
    ;; just return the existing bus server. Creating a bus server
    ;; should succeed unless the port is in use.
    (let+ ((promise (lparallel:promise))
           ((&flet ensure-bus (connector)
              (values (let ((*port-promise* promise))
                        (transport-ensure-bus
                         transport :server! connector address))
                      (lparallel:force promise))))
           ((&values bus-1 port-1)
            (ensure-bus connector-1))
           ((&values bus-2 port-2)
            (ensure-bus connector-2)))
      ;; Make sure that both connectors got the same port via their
      ;; respective "portfile" outputs.
      (ensure-same port-1 port-2 :test #'=)

      (check-buses-and-connectors
       (list bus-1 bus-2) (list connector-1 connector-2)))))
