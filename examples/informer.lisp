;;;; informer.lisp --- An example program demonstrating the informer.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; For managing the lifetime of informers (e.g. for short-lived
;; informers), the `with-participant' macro can used. It will take
;; care of disposing of the `informer' instance after it has been
;; used, also in case of non-local exist.
;; mark-start::with-participant
(rsb:with-participant (informer :informer "/example/informer"
                                :type 'string)
  (format t "Sending first event~%")
  (rsb:send informer "example payload"))
;; mark-end::with-participant

;; The following code will create an `informer' instance that
;; publishes events to the channel designated by the scope
;; "/example/informer" and is restricted to event payloads of type
;; string. The informer will use all transports which are enabled in
;; the configuration with their respective configured options.
;;
;; This will publish the string "data" to the channel in which the
;; informer participates.
;;
;; mark-start::variable
(defvar *informer* (rsb:make-participant :informer "/example/informer"
                                         :type 'string))

(format t "Sending second event~%")
(rsb:send *informer* "example payload")

;; The informer will participate in the channel until it is garbage
;; collected or explicitly detached using the `rsb:detach' function.

(rsb:detach *informer*)
;; mark-end::variable

;; mark-end::body
