;;;; types.lisp --- Types used in the cl-rsb system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb)

;;; Scope-related types

(deftype scope-component ()
  '(and string (satisfies scope-component?)))

(deftype scope-components ()
  '(or null
       (cons scope-component t)))

(defun scope-component? (string)
  "Non-nil when STRING is a valid scope component."
  (let+ (((&flet valid-char? (char)
            (or (char<= #\a char #\z)
                (char<= #\A char #\Z)
                (char<= #\0 char #\9)
                (char= char #\_) (char= char #\-)))))
    (declare (dynamic-extent #'valid-char?))
    (and (stringp string) (not (emptyp string))
         (every #'valid-char? string))))

(deftype scope-designator ()
  "A scope can be designated by a string, a list of scope components
or a `scope' instance."
  '(or string scope-components scope))

;;; Event-related types

(deftype sequence-number ()
  "Event sequence numbers are 32-bit unsigned integers."
  '(unsigned-byte 32))

(deftype event-id ()
  "A pair of an origin id and sequence that uniquely identifies and
event."
  '(cons uuid:uuid sequence-number))

(deftype timestamp-designator ()
  "Name of a timestamp."
  'keyword)

(deftype meta-data-designator ()
  "Name of a meta-data item."
  'keyword)

;;; Event-processing-related types

(deftype error-policy ()
  "Objects of this type designate behaviors in case of errors."
  '(or null function))

(deftype timeout ()
  "Amount of seconds to wait before a timeout should occur."
  'non-negative-real)

(deftype implementation-feedback ()
  '(member :implemented :not-implemented))

(deftype direction ()
  '(member :in-push :in-pull :out))

(deftype wire-type ()
  "A certain type of data exchanged \"on the wire\" of a transport
mechanism."
  '(or symbol list))
