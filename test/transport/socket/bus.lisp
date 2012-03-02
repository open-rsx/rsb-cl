;;; bus.lisp --- Unit tests for the bus* classes.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.transport.socket.test)

(deftestsuite transport-socket-bus-root (transport-socket-root)
  (deadlock-detector)
  (:function
   (make-connector (class host port server?)
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
   (sb-ext:without-package-locks
     (setf (fdefinition 'sb-thread::check-deadlock) deadlock-detector)))
  (:documentation
   "Unit tests for the `bus', `bus-client' and `bus-server'
classes."))

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
	   (connector-1 (make-connector connector-class host port nil))
	   (connector-2 (make-connector connector-class host port nil)))

      ;; There is no server yet, so this has to signal an error.
      (ensure-condition 'usocket:connection-refused-error ;;; TODO(jmoringe): keep this condition type?
	(ensure-bus-client host port connector-1))

      ;; Create a bus server.
      (with-reader (dummy (make-socket-url t) :transports '((:socket :converter :fundamental-null)))
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
	   (connector-1 (make-connector connector-class host port t))
	   (connector-2 (make-connector connector-class host port t)))

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
