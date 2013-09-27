;;;; informer.lisp --- An example program demonstrating the informer.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; For managing the lifetime of informers (e.g. for short-lived
;; informers), the `with-informer' macro can used. It will take care
;; of disposing of the `informer' instance after it has been used,
;; also in case of non-local exist.
;; mark-start::with-informer
(rsb:with-informer (informer "/example/informer" 'string)
  (rsb:send informer "example payload"))
;; mark-end::with-informer

;; The following code will create an `informer' instance that
;; publishes events to the channel designated by the scope
;; "/example/informer" and is restricted to event payloads of type
;; string. The informer will use all transports which are enabled in
;; the configuration with their respective configured options.
;;
;; This will publish the string "data" to the channel in which the
;; informer participates.
;;
;; The informer will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.
;; mark-start::variable
(defvar *informer* (rsb:make-informer "/example/informer" 'string))

(rsb:send *informer* "example payload")
;; mark-end::variable
;; mark-end::body
