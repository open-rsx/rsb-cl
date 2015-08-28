;;;; package.lisp --- Package definition for the model.inference module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.inference
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb.model)

  (:import-from #:rsb
   #:scope
   #:scope-string
   #:scope=
   #:sub-scope?

   #:uri->scope-and-options)

  (:export
   #:communication?
   #:participants-communicate-using-kinds?)

  (:documentation
   "This package contains functions for inferring facts about modeled
    systems.

    The following inference protocols are available:

    * communication? from to        [generic function]

      Determine whether communication between the objects represented
      by model objects FROM and TO is possible."))
