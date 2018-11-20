;;;; protocol.lisp --- Protocol functions provided by the patterns.reader package..
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.reader)

;;; Reader protocol

(defgeneric receive (reader
                     &key
                     block?)
  (:documentation
   "Receive data from the channel in which READER is participating.

    When data is received, it is returned in form of an `event'
    instance.

    If BLOCK? is non-nil, wait for data to become available if there
    is none. If BLOCK? is nil and no data is available, nil is
    returned."))
