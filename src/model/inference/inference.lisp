;;;; inference.lisp --- Implementation of the model.inference module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.inference)

;;; Scope- and transport-level inference

(defmethod communication? ((from scope) (to scope))
  (cond
    ;; Sender sends on sub-scope of receiver's scope => Receiver
    ;; definitely receives events.
    ((sub-scope? from to) (values t   t))
    ;; Sender sends on super-scope of receiver's scope => Receiver may
    ;; receive events when sender sends on a suitable sub-scope of its
    ;; scope (which is allowed but may not necessarily happen).
    ((sub-scope? to from) (values nil nil))
    ;; None of the above relations hold => Receiver can definitely not
    ;; receive events sent by sender.
    (t                    (values nil t))))

;; We cannot be certain if one thing is a URI and the other thing is a
;; scope.
(defmethod communication? ((from scope) (to puri:uri))
  (tri-and (communication? from (uri->scope-and-options to))
           (values nil nil)))

(defmethod communication? ((from puri:uri) (to scope))
  (tri-and (communication? (uri->scope-and-options from) to)
           (values nil nil)))

(defmethod communication? ((from puri:uri) (to puri:uri))
  (let+ (((&values to-scope to-options)
          (uri->scope-and-options to))
         ((&values from-scope from-options)
          (uri->scope-and-options from))
         ;; TODO when we get first-class transports, there will be a
         ;; generic function in the transport protocol for deciding
         ;; whether, given a certain transport, two sets of transport
         ;; options for that transport allow for communication.
         ((&flet options-plist (options)
            (remove '&inherit (rest options))))
         ((&flet enabled? (options)
            (getf (options-plist options) :enabled)))
         ((&flet+ same-transport? ((&whole left  left-transport  &rest &ign)
                                   (&whole right right-transport &rest &ign))
            (and (eq left-transport right-transport)
                 (equal (options-plist left)
                        (options-plist right)))))
         ((&flet+ common-transport? (left-options right-options)
            (map-product (lambda (left right)
                           (when (same-transport? left right)
                             (return-from common-transport?
                               (values t t))))
                         (remove-if-not #'enabled? left-options)
                         (remove-if-not #'enabled? right-options))
            (values nil t))))
    (tri-and (communication? from-scope to-scope)
             (common-transport? to-options from-options))))

;;; Participant-level inference
;;;
;;; This level takes into account participant kinds, scopes and
;;; transport configurations but not the question whether child
;;; participants communicate.

(defmethod communication? ((from participant-info) (to participant-info))
  (participants-communicate-using-kinds?
   from to (participant-info-kind from) (participant-info-kind to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind t)
     (to-kind   t))
  (values nil nil))

;; `:%sender' and `:%receiver' are pseudo-participant kinds that allow
;; the definition of this basic method. Methods for real participant
;; kinds can call this method to check scope and transport
;; communication.
(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :%sender))
     (to-kind   (eql :%receiver)))
  (let+ (((&structure-r/o
           participant-info- (from-scope scope) (from-transports transports))
          from)
         ((&structure-r/o
           participant-info- (to-scope   scope) (to-transports   transports))
          to)
         ((&flet scope+uri->uri (scope uri)
            (puri:merge-uris (make-instance 'puri:uri
                                            :path (scope-string scope))
                             uri)))
         (uris '())
         ((&flet communication?/transports (from-uri to-uri)
            (let+ (((&values value definitive?)
                    (communication? (scope+uri->uri from-scope from-uri)
                                    (scope+uri->uri to-scope   to-uri))))
              (when (or value (not definitive?))
                (push (cons from-uri to-uri) uris))
              (values value definitive?)))))
    ;; TODO special hack until all languages support this
    (when (or (not from-transports) (not to-transports))
      (return-from participants-communicate-using-kinds?
        (communication? from-scope to-scope)))

    ;; Check for all combinations of transports whether communication
    ;; is possible.
    (multiple-value-call #'values
      (cond
        ;; Optimization for common case of a one transport in each info.
        ((and (length= 1 from-transports) (length= 1 to-transports))
         (communication?/transports
          (first from-transports) (first to-transports)))
        ;; Hopefully uncommon case: check all combinations of transports
        (t
         (tri-reduce #'%tri-or
                     (map-product (lambda (from-uri to-uri)
                                    (multiple-value-list
                                     (communication?/transports from-uri to-uri)))
                                  from-transports to-transports)
                     :initial-value '(nil t))))
      uris)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :listener))
     (to-kind   t))
  (values nil t))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind t)
     (to-kind   (eql :informer)))
  (values nil t))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :informer))
     (to-kind   (eql :listener)))
  (participants-communicate-using-kinds? from to :%sender :%receiver))

(macrolet
    ((define-communicate-using-kinds?-method (from-kind to-kind)
       `(defmethod participants-communicate-using-kinds?
            ((from      participant-info)
             (to        participant-info)
             (from-kind (eql ,from-kind))
             (to-kind   (eql ,to-kind)))
          ;; Methods need identical scopes in addition to the usual
          ;; requirements.
          (if (scope= (participant-info-scope from)
                      (participant-info-scope to))
              (participants-communicate-using-kinds?
               from to :%sender :%receiver)
              (values nil t)))))
  (define-communicate-using-kinds?-method :local-method  :remote-method)
  (define-communicate-using-kinds?-method :remote-method :local-method))

;;; Composite-level inference

(defmethod communication? ((from node) (to node))
  (let+ ((uris '())
         ((&flet+ uri-pair= ((left-from  . left-to)
                             (right-from . right-to))
            (and (puri:uri= left-from right-from)
                 (puri:uri= left-to   right-to))))
         ((&flet communication?/push-uris (from to)
            (let+ (((&values value definitive? uris*)
                    (communication? from to)))
              (when (or value (not definitive?))
                (dolist (uri uris*)
                  (pushnew uri uris :test #'uri-pair=)))
              (values value definitive?)))))
    (multiple-value-call #'values
      (tri-or (communication?/push-uris (node-info from) (node-info to))
              (tri-some (rcurry #'communication?/push-uris to)
                        (node-children from))
              (tri-some (curry #'communication?/push-uris from)
                        (node-children to)))
      uris)))
