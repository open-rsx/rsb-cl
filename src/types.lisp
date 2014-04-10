;;;; types.lisp --- Types used in the cl-rsb system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Scope-related types

(declaim (inline scope-component-character?))
(defun scope-component-character? (character)
  "Non-nil when character is valid within a scope component."
  (or (char<= #\a character #\z)
      (char<= #\A character #\Z)
      (char<= #\0 character #\9)
      (char= character #\_) (char= character #\-)))
(declaim (notinline scope-component-character?))

(deftype scope-component-character ()
  '(and character (satisfies scope-component-character?)))

(defun scope-component? (string)
  "Non-nil when STRING is a valid scope component."
  (locally (declare (inline scope-component-character?))
    (and (stringp string)
         (plusp (length string))
         (every #'scope-component-character? string))))

(deftype scope-component ()
  '(and string (satisfies scope-component?)))

(defun scope-components? (sequence)
  (every #'scope-component? sequence))

(deftype scope-components ()
  '(and sequence (satisfies scope-components?)))

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
