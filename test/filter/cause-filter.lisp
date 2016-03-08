;;;; cause-filter.lisp --- Unit tests for the cause-filter class.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite cause-filter-root (filter-root)
  ()
  (:documentation
   "Test suite for the `cause-filter' class."))

(define-basic-filter-tests (cause-filter :cause)
  `(;; Some invalid cases.
    (()                                                         error) ; missing initarg
    ((:cause 5)                                                 error) ; wrong cause for cause specifier
    ((:cause "00000000-0000-0000-0000-")                        error) ; UUID syntax error
    ((:cause "00000000-0000-0000-0000-000000000000"
      :origin"00000000-0000-0000-0000-000000000000")
                                                                error) ; incompatible initargs
    ((:cause           "00000000-0000-0000-0000-000000000000"
      :sequence-number 0)
                                                                error) ; incompatible initargs

    ;; These are ok.
    ((:cause t)                                                 t)

    ((:cause ,(uuid:make-null-uuid))                            t)
    ((:cause "00000000-0000-0000-0000-000000000000")            t)
    ((:cause ,(uuid:uuid-to-byte-array (uuid:make-null-uuid)))  t)
    ((:cause ,(cons (uuid:make-null-uuid) 0))                   t)

    ((:origin ,(uuid:make-null-uuid))                           t)
    ((:origin "00000000-0000-0000-0000-000000000000")           t)
    ((:origin ,(uuid:uuid-to-byte-array (uuid:make-null-uuid))) t)

    ((:sequence-number 0)                                       t)))

(define-filter-match-test (cause-filter :cause)
  `(;; Events without UUID
    ((:cause t)                      ("/" nil) nil)

    ((:cause ,(uuid:make-null-uuid)) ("/" nil) nil)
    ((:cause ,(uuid:make-v4-uuid))   ("/" nil) nil)

    ;; Events with UUID
    ((:cause t)
     ("/" nil :causes (,(cons (uuid:make-v4-uuid) 0)))
     t)

    ((:cause ,(uuid:make-v4-uuid))
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     nil)
    ((:cause ,(event-id->uuid (cons (uuid:make-null-uuid) 0)))
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     t)
    ((:cause ,(cons (uuid:make-null-uuid) 1))
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     nil)
    ((:cause ,(cons (uuid:make-null-uuid) 1))
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 1)))
     t)
    ((:origin           ,(uuid:make-null-uuid)
      :sequence-number 0)
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     t)

    ((:origin ,(uuid:make-null-uuid))
     ("/" nil :causes (,(cons (uuid:make-v4-uuid) 0)))
     nil)
    ((:origin ,(uuid:make-null-uuid))
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     t)

    ((:sequence-number 1)
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 0)))
     nil)
    ((:sequence-number 1)
     ("/" nil :causes (,(cons (uuid:make-null-uuid) 1)))
     t)))
