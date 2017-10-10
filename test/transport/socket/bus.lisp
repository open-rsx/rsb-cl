;;;; bus.lisp --- Unit tests for the bus* classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

(defun make-socket-connector (class schema address
                              &rest options &key server? &allow-other-keys)
  (let ((connector (apply #'make-instance class
                          :schema    schema
                          :converter :fundamental-null
                          :server?   server?
                          (append address
                                  (remove-from-plist options :server?)))))
    (when (typep connector 'rsb.transport.socket::in-connector)
      (setf (rsb.transport.socket::connector-scope connector)
            (make-scope "/rsbtests/transport/socket/bus/")))
    connector))

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

(defun check-buses-and-connectors (buses connectors
                                   &optional expect-connection?)
  (let ((expected-connection-count (if expect-connection? 1 0)))
    ;; Make sure that both connectors got the same bus server.
    (loop :for (bus-1 bus-2) :on buses :do
       (check-bus bus-1 expected-connection-count connectors)
       (when bus-2
         (ensure-same bus-1 bus-2 :test #'eq)))

    ;; Detach connectors one-by-one and check the resulting state.
    (let ((bus (first buses)))
      (loop :for (connector . rest) :on connectors :do
         (notify connector bus :detached)
         (check-bus bus (if rest expected-connection-count 0) rest)))))
