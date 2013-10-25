;;;; package.lisp --- Package definition for patterns.data-flow module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.data-flow
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; conditions
  (:export
   #:flow-condition

   #:send-while-suspended

   #:remote-flow-condition

   #:water-mark-condition

   #:high-watermark-reached #:high-watermark-reached/remote
   #:low-watermark-reached  #:low-watermark-reached/remote)

  ;; `source' participant class
  (:export
   #:source
   #:make-source)

  ;; `sink' participant class
  (:export
   #:sink
   #:make-sink)

  (:documentation
   "This package contains data-flow-related patterns."))
