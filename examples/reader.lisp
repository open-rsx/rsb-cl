;;;; reader.lisp --- An example program demonstrating the reader.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body

;; For managing the lifetime of readers (e.g. for short-lived
;; readers), the `with-reader' macro can used. It will take care of
;; disposing of the `reader' instance after it has been used, also in
;; case of non-local exist.
;;
;; Note: this form will block until an event is received.
;; mark-start::with-reader
(rsb:with-reader (reader "/example/informer")
  (let ((event (rsb:receive reader)))
    (format t "Received event: ~A~%" event)
    event)) ;; return the event
;; mark-end::with-reader

;; This will create a `reader' instance that receives events which are
;; sent to the channel designated by the scope "/example/reader". The
;; reader will use all transports which are enabled in the
;; configuration with their respective configured options.
;;
;; Note: the `receive' call will block until an event is received.
;; mark-start::variable
(defvar *reader* (rsb:make-reader "/example/informer"))

;; The reader will participate in the channel until it is garbage
;; collected or explicitly detached from the channel.

;; mark-start::receive/block
(let ((event (rsb:receive *reader* :block? t))) ;; block? defaults to t
  (format t "Received event: ~A~%" event)
  event) ;; return the event
;; mark-end::receive/block

;; It is also possible to use `rsb:receive' in a non-blocking mode. In
;; that case, an `event' or nil is returned.
;; mark-start::receive/noblock
(let ((event (rsb:receive *reader* :block? nil)))
  (format t "~:[Did not receive an event~;Received event: ~:*~A~]~%"
	  event)
  event) ;; return the event, or nil
;; mark-end::receive/noblock
;; mark-end::variable

;; mark-end::body
