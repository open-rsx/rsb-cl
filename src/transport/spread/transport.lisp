;;;; transport.lisp --- Spread transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(register-transport
 :spread
 :schemas   :spread
 :wire-type 'nibbles:octet-vector
 :remote?   t
 :documentation
 "A transport implementation using the Spread group communication framework.

  This transport maps scopes to Spread groups to allow multicast-based
  distribution of events to interested processes.")
