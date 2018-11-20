;;;; reader.lisp --- An example program demonstrating the reader.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Loading this file does not terminate.

;; mark-start::body
;; For managing the lifetime of readers (e.g. for short-lived
;; readers), the `with-participant' macro can used. It will take care
;; of disposing of the `reader' instance after it has been used, also
;; in case of non-local exist.
;;
;; Note: this form will block until an event is received.
;; mark-start::with-participant
(rsb:with-participant (reader :reader "/example/informer")
  (let ((event (rsb.patterns.reader:receive reader)))
    (format t "Received event: ~A~%" event)
    event)) ; return the event
;; mark-end::with-participant

;; This will create a `reader' instance that receives events which are
;; sent to the channel designated by the scope "/example/reader". The
;; reader will use all transports which are enabled in the
;; configuration with their respective configured options.
;;
;; Note: the `receive' call will block until an event is received.
;;
;; mark-start::variable
(defvar *reader* (rsb:make-participant :reader "/example/informer"))

;; mark-start::receive/block
(let ((event (rsb.patterns.reader:receive *reader* :block? t))) ; block? defaults to t
  (format t "Received event: ~A~%" event)
  event) ; return the event
;; mark-end::receive/block

;; It is also possible to use `rsb.patterns.reader:receive' in a
;; non-blocking mode. In that case, an `event' or nil is returned.
;; mark-start::receive/noblock
(let ((event (rsb.patterns.reader:receive *reader* :block? nil)))
  (format t "~:[Did not receive an event~;Received event: ~:*~A~]~%"
          event)
  event) ; return the event, or nil
;; mark-end::receive/noblock

;; The reader will participate in the channel until it is garbage
;; collected or explicitly detached using the `rsb:detach' function.

(rsb:detach *reader*)
;; mark-end::variable

;; mark-end::body
