;;;; bus.lisp --- Unit tests for the bus* classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.socket.test)

(deftestsuite transport-socket-bus-root (transport-socket-root)
  (deadlock-detector)
  (:function
   (make-socket-connector (class host port server?)
     (apply #'make-instance class
            :host      host
            :port      port
            :converter :fundamental-null
            (when server?
              '(:server? t)))))
  #+sbcl
  (:setup
   ;; As a workaround for https://bugs.launchpad.net/asdf/+bug/507378,
   ;; force `receive-messages' to be updated.
   (ignore-some-conditions (sb-ext:timeout)
     (sb-ext:with-timeout .1
       (receive-messages (make-instance 'rsb.transport.test::mock-receiver))))
   (usocket:with-socket-listener (socket "localhost" *next-port*)
     (map nil #'usocket:socket-close
          (list (usocket:socket-connect "localhost" *next-port*)
                (usocket:socket-accept socket))))
   ;; Disable deadlock detection since it seems to produce bogus
   ;; detections when interrupt-thread is used.
   (format *lift-debug-output* "~&;; Disabling SBCL deadlock detector~&")
   (sb-ext:without-package-locks
     (setf deadlock-detector (fdefinition 'sb-thread::check-deadlock))
     (defun sb-thread::check-deadlock ())))
  #+sbcl
  (:teardown
   (format *lift-debug-output* "~&;; Restoring SBCL deadlock detector~&")
   (when deadlock-detector
     (sb-ext:without-package-locks
       (setf (fdefinition 'sb-thread::check-deadlock) deadlock-detector))))
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
  (ensure-condition reader-creation-failed
    (with-reader (r1 (make-socket-url t nil))
      (with-reader (r2 (make-socket-url nil '("tcpnodelay" "0")))
        (with-reader (r3 (make-socket-url nil nil))
          ;; body is not important
          )))))

(addtest (transport-socket-bus-root
          :documentation
          "Test creating `bus-client' instances and attaching and
detaching connectors to them. Try multiple connectors of different
classes which also causes the test case to be repeated without a fresh
port. This helps ensuring proper cleanup.")
  smoke/client

  (ensure-cases (connector-class)
      (mappend #'(lambda (class) (make-list 10 :initial-element class))
              '(in-pull-connector in-push-connector out-connector))

    (let* ((host        "localhost")
           (port        *next-port*)
           (connector-1 (make-socket-connector connector-class host port nil))
           (connector-2 (make-socket-connector connector-class host port nil)))

      ;; There is no server yet, so this has to signal an error.
      (ensure-condition 'usocket:connection-refused-error ; TODO(jmoringe): keep this condition type?
        (ensure-bus-client host port connector-1))

      ;; Create a bus server.
      (with-reader (dummy (make-socket-url t nil) :converters '((t . :fundamental-null)))
        ;; We should be able to create a bus clients now. We create
        ;; two connectors and request a bus client for each of
        ;; them. The first request should cause the bus client to be
        ;; created, while the second should just return the existing
        ;; bus client.
        (let ((bus-1 (ensure-bus-client host port connector-1))
              (bus-2 (ensure-bus-client host port connector-2)))
          ;; Make sure that both connectors got the same bus client.
          (check-bus bus-1 1 (list connector-1 connector-2))
          (check-bus bus-2 1 (list connector-1 connector-2))
          (ensure-same bus-1 bus-2 :test #'eq)

          ;; Detach connector-1 and check the resulting state.
          (notify connector-1 bus-1 :detached)
          (check-bus bus-1 1 (list connector-2))

          ;; Detach connector-2 and check the resulting
          ;; state. Detaching connector-2 (since it is the last
          ;; remaining connector) should cause the connection to be
          ;; disconnected.
          (notify connector-2 bus-1 :detached)
          (check-bus bus-1 0 0))))))

(addtest (transport-socket-bus-root
          :documentation
          "Test creating `bus-server' instances and attaching and
detaching connectors to them. Try multiple connectors of different
classes which also causes the test case to be repeated without a fresh
port. This helps ensuring proper cleanup.")
  smoke/server

  (ensure-cases (connector-class)
      (mappend #'(lambda (class) (make-list 10 :initial-element class))
               '(in-pull-connector in-push-connector out-connector))

    (let* ((host        "localhost")
           (port        *next-port*)
           (connector-1 (make-socket-connector connector-class host port t))
           (connector-2 (make-socket-connector connector-class host port t)))

      ;; Create two connectors and request a bus server for each of
      ;; them. The first request should cause the bus server to be
      ;; created, while the second should just return the existing bus
      ;; server. Creating a bus server should succeed unless the port
      ;; is in use.
      (let ((bus-1 (ensure-bus-server host port connector-1))
            (bus-2 (ensure-bus-server host port connector-2)))
        ;; Make sure that both connectors got the same bus server.
        (check-bus bus-1 0 (list connector-1 connector-2))
        (check-bus bus-2 0 (list connector-1 connector-2))
        (ensure-same bus-1 bus-2 :test #'eq)

        ;; Detach connector-1 and check the resulting state.
        (notify connector-1 bus-1 :detached)
        (check-bus bus-1 0 (list connector-2))

        ;; Detach connector-2 and check the resulting state.
        (notify connector-2 bus-1 :detached)
        (check-bus bus-1 0 0)))))
