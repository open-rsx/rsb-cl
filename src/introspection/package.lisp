;;;; package.lisp --- Package definition for the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection
  (:use
   #:cl
   #:alexandria

   #:rsb)

  ;; Conditions
  (:export
   #:introspection-protocol-error)

  ;; Variables
  (:export
                                      #:+introspection-scope+

   #:introspection-participants-scope #:+introspection-participants-scope+
   #:participant-id->scope
   #:scope->participant-id-or-nil
   #:scope->participant-id

   #:introspection-hosts-scope        #:+introspection-hosts-scope+

   #:introspection-process-scope)

  (:documentation
   "This package contains the introspection functionality.

    There are two aspects:

    1) Broadcasting pieces of introspection information describing the
       local host, the current process and participants in it.

    2) Receiving this information and aggregating the individual
       pieces into a global view of the system.

    1) should happen in almost all RSB applications while 2) is mostly
    relevant for tools which inspect and describe system.

    Both aspects share a model protocol and model classes to
    handle (pieces of) introspection information. This model consists
    of the classes:

    * `participant-info' and `remote-participant-info' which describe
      participants in general and participants in remote processes
      respectively.

    * `process-info' and `remote-process-info' which describe
      processes in general and processes on remote hosts respectively.

    * `host-info' and `remote-host-info' which describe hosts in
      general and in remote hosts respectively."))
