;;;; bus.lisp --- Unit tests for the bus* classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

(defun make-socket-connector (class host port &key server? portfile)
  (apply #'make-instance class
         :host      host
         :port      port
         :converter :fundamental-null
         (append (when server?
                   '(:server? t))
                 (when portfile
                   `(:portfile ,portfile)))))

(defun check-bus (bus expected-connections expected-connectors)
  (flet ((check-thing (title reader expected)
           (etypecase expected
             (list
              (ensure-same (funcall reader bus) expected
                           :test (rcurry #'set-equal :test #'eq)))
             (number
              (let ((num (length (funcall reader bus))))
                (ensure-same num expected
                             :test      #'=
                             :report    "~@<Bus was expected to have ~
                                         ~D ~(~A~)~:P (not ~D)~:@>"
                             :arguments (expected title num)))))))
    ;; Ensure that connections of BUS match EXPECTED-CONNECTIONS.
    (check-thing :connection #'bus-connections expected-connections)
    ;; Ensure that connectors of BUS match EXPECTED-CONNECTORS.
    (check-thing :connector  #'bus-connectors  expected-connectors)))

(defun check-buses-and-connectors (bus-1 bus-2 connector-1 connector-2
                                   expect-connection?)
  ;; Make sure that both connectors got the same bus server.
  (check-bus bus-1 (if expect-connection? 1 0) (list connector-1 connector-2))
  (check-bus bus-2 (if expect-connection? 1 0) (list connector-1 connector-2))
  (ensure-same bus-1 bus-2 :test #'eq)

  ;; Detach connector-1 and check the resulting state.
  (notify connector-1 bus-1 :detached)
  (check-bus bus-1 (if expect-connection? 1 0) (list connector-2))

  ;; Detach connector-2 and check the resulting state.
  (notify connector-2 bus-1 :detached)
  (check-bus bus-1 0 0))

(deftestsuite transport-socket-bus-root (transport-socket-root)
  ()
  (:documentation
   "Unit tests for the `bus', `bus-client' and `bus-server'
    classes."))

(addtest (transport-socket-bus-root
          :documentation
          "Test supplying incompatible options for a single
connection.")
  incompatible-options

  ;; Has to signal an error since the incompatible options are
  ;; supplied for a single host-port combination.
  (ensure-condition participant-creation-error
    (with-participant (r1 :reader (make-socket-url t nil))
      (declare (ignore r1))
      (with-participant (r2 :reader (make-socket-url nil '("tcpnodelay" "0")))
        (declare (ignore r2))
        (with-participant (r3 :reader (make-socket-url nil nil))
          (declare (ignore r3))))))) ; body is not important

(addtest (transport-socket-bus-root
          :documentation
          "Test creating `bus-client' instances and attaching and
           detaching connectors to them. Try multiple connectors of
           different classes which also causes the test case to be
           repeated without a fresh port. This helps ensuring proper
           cleanup.")
  client/smoke

  (ensure-cases (connector-class)
      (mappend (lambda (class) (make-list 10 :initial-element class))
              '(in-pull-connector in-push-connector out-connector))

    (let* ((host        "localhost")
           (port        *next-port*)
           (connector-1 (make-socket-connector connector-class host port))
           (connector-2 (make-socket-connector connector-class host port)))

      ;; There is no server yet, so this has to signal an error.
      (ensure-condition 'usocket:connection-refused-error ; TODO(jmoringe): keep this condition type?
        (ensure-bus-client host port connector-1))

      ;; Create a bus server.
      (with-participant (dummy :reader (make-socket-url t nil)
                               :converters '((t . :fundamental-null)))
        ;; We should be able to create a bus clients now. We create
        ;; two connectors and request a bus client for each of
        ;; them. The first request should cause the bus client to be
        ;; created, while the second should just return the existing
        ;; bus client.
        (let ((bus-1 (ensure-bus-client host port connector-1))
              (bus-2 (ensure-bus-client host port connector-2)))
          (check-buses-and-connectors bus-1 bus-2 connector-1 connector-2 t))))))

(addtest (transport-socket-bus-root
          :documentation
          "Test creating `bus-server' instances and attaching and
           detaching connectors to them. Try multiple connectors of
           different classes which also causes the test case to be
           repeated without a fresh port. This helps ensuring proper
           cleanup.")
  server/smoke

  (ensure-cases (connector-class)
      (mappend (lambda (class) (make-list 10 :initial-element class))
               '(in-pull-connector in-push-connector out-connector))

    (let* ((host        "localhost")
           (port        *next-port*)
           (connector-1 (make-socket-connector connector-class host port
                                               :server? t))
           (connector-2 (make-socket-connector connector-class host port
                                               :server? t)))

      ;; Create two connectors and request a bus server for each of
      ;; them. The first request should cause the bus server to be
      ;; created, while the second should just return the existing bus
      ;; server. Creating a bus server should succeed unless the port
      ;; is in use.
      (let ((bus-1 (ensure-bus-server host port connector-1))
            (bus-2 (ensure-bus-server host port connector-2)))
        (check-buses-and-connectors bus-1 bus-2 connector-1 connector-2 nil)))))

(addtest (transport-socket-bus-root
          :documentation
          "Test automatically assigned ports and the portfile
           option.")
  server/automatic-port

  (let* ((host        "localhost")
         (port        0)
         (connector-1 (make-socket-connector
                       'connector host port :server? t :portfile "-"))
         (connector-2 (make-socket-connector
                       'connector host port :server? t :portfile "-2")))

    ;; Create two connectors and request a bus server with automatic
    ;; port assignment for each of them. The first request should
    ;; cause the bus server to be created, while the second should
    ;; just return the existing bus server. Creating a bus server
    ;; should succeed unless the port is in use.
    (let+ (((&flet ensure-bus (connector stream-name)
              (let+ ((bus)
                     ((&flet make-bus (stream)
                        (progv (list stream-name) (list stream)
                          (setf bus (ensure-bus-server
                                     host port connector)))))
                     (port (with-output-to-string (stream)
                             (make-bus stream))))
                (values bus port))))
           ((&values bus-1 port-1)
            (ensure-bus connector-1 '*standard-output*))
           ((&values bus-2 port-2)
            (ensure-bus connector-2 '*error-output*)))
      ;; Make sure that both connectors got the same port via their
      ;; respective "portfile" outputs.
      (ensure-same port-1 port-2 :test #'string=)

      (check-buses-and-connectors bus-1 bus-2 connector-1 connector-2 nil))))
