;;;; protocol.lisp --- Protocol provided by the model.inference module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.inference)

;;; Communication inference protocol
;;;
;;; The functions in this protocol return information about the
;;; possibility of communication between RSB objects (hosts,
;;; processes, participants, etc.).
;;;
;;; The returned information represents a static view of the situation
;;; in the following sense: communication between objects is
;;; considered possible when the respective transport configuration,
;;; scopes, filters, etc. allow for communication between the
;;; objects. This does not imply that communication actually happens
;;; or will happen.
;;;
;;; There is the additional notion of possible communication when
;;; certain criteria are met. For example, an informer on scope /foo/
;;; is allowed to publish events on scope /foo/bar/. Whether a
;;; listener on scope /foo/bar/ receives events from that informer
;;; therefore depends on whether the informer actually publishes on
;;; such a sub-scope. This notion is the reason for tri-modal return
;;; values of `communication?' and related functions.

(defgeneric communication? (from to)
  (:documentation
   "Determine whether communication between FROM and TO is possible.

    Return three values, the first two of which indicate a ternary
    result (like `cl:subtypep'):

      first value | second value | meaning
      t           | t            | communication is definitely possible
      nil         | t            | communication is definitely impossible
      nil         | nil          | cannot determine whether communication
                  |              | is possible

    When present, the third value is a list of pairs of the form

      (FROM-URI . TO-URI)

    where FROM-URI is associated to FROM and TO-URI is associated to
    TO and communication between FROM and TO is possible via the
    indicated transports.

    FROM and TO can describe various aspects of an RSB system such as
    URIs, scopes, participants (via `rsb.model:participant-info') or
    processes (via `rsb.model:process-info')."))

(defgeneric participants-communicate-using-kinds? (from to from-kind to-kind)
  (:documentation
   "Determine whether communication between FROM and TO is possible.

    Return values are identical to `communication?'.

    FROM and TO are `rsb.model:participant-info' instances of kinds
    FROM-KIND and TO-KIND respectively.

    This function is mainly used as a helper function of
    `communication?' when answering questions about participants."))

;; Default behavior

(defmethod communication? ((from t) (to t))
  (values nil nil))
