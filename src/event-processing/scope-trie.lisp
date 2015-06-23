;;;; scope-trie.lisp --- [sink-]scope-trie data structures.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; A trie data structure in which paths consist of scope
;;;; components. Accesses are lock-free with atomic updates.

(cl:in-package #:rsb.event-processing)

(define-constant +no-value+ '%no-value)

;;; Scope trie data structure

(declaim (inline make-node-state make-empty-node-state))
(defstruct (node-state
             (:constructor make-node-state (value edges))
             (:predicate   nil)
             (:copier      nil))
  (value +no-value+            :read-only t)
  (edges ()         :type list :read-only t))

(defun make-empty-node-state ()
  (make-node-state +no-value+ '()))

(declaim (inline make-node))
(defstruct (node
            (:constructor make-scope-trie ())
            (:constructor make-node (state))
            (:predicate   nil)
            (:copier      nil))
  (state (make-empty-node-state) :type (or null node-state)))

(deftype scope-trie () 'node)

;; Call FUNCTION with TRIE and its children on the path described by
;; COMPONENTS.
;;
;; EXTEND? controls whether nodes on the path should be added to TRIE
;; if they do not already exist.
;;
;; LEAF-ONLY? controls whether FUNCTION is called for all nodes
;; corresponding to prefixes of COMPONENTS or only for the node
;; corresponding to the full path COMPONENTS.
(defun scope-trie-%map (function components trie extend? leaf-only?)
  (declare (type function function)
           (type list components)
           (type node trie))
  (tagbody
     ;; We restart here when encountering an in-progress deletion.
   :global-start
     (let+ ((deleting?)
            ((&labels+ visit ((&optional component &rest rest) node)
               (tagbody
                  ;; We restart here in case CASing the node state
                  ;; fails, redoing all computations to construct the
                  ;; new node state.
                :start
                  (let* ((old-state (or (node-state node)
                                        ;; Restart if NODE being
                                        ;; deleted.
                                        (if deleting?
                                            (return-from visit node)
                                            (go :global-start))))
                         ;; Call FUNCTION if inner nodes have been
                         ;; requested or we have reached the end of
                         ;; COMPONENTS.
                         (new-state (if (or (not component) (not leaf-only?))
                                        (funcall function old-state)
                                        old-state))
                         (old-value (node-state-value (or new-state old-state)))
                         (old-edges (node-state-edges (or new-state old-state)))
                         (next))
                    (cond
                      ;; End of COMPONENTS => stop traversal.
                      ((not component))
                      ;; A node corresponding to COMPONENT does
                      ;; already exist => descend into that node.
                      ((when-let ((cell (assoc component old-edges
                                               :test #'string=)))
                         (setf next (cdr cell))))
                      ;; Such a node does not exist, but we are
                      ;; supposed to extend the trie => create a new
                      ;; node.
                      (extend?
                       (let ((new-child (make-node (make-empty-node-state))))
                         (setf new-state (make-node-state
                                          old-value (list* (cons component
                                                                 new-child)
                                                           old-edges))
                               next      new-child))))
                    ;; if we have a next node to visit, do it
                    ;; (including ancestors), then check whether it
                    ;; should be deleted.
                    (when next
                      (let ((child (visit rest next)))
                        (unless (node-state child)
                          (setf deleting? t)
                          (let ((new-edges (remove child old-edges
                                                   :test #'eq :key #'cdr
                                                   :count 1)))
                            (setf new-state
                                  (when (or (not (eq old-value +no-value+))
                                            new-edges)
                                    (make-node-state old-value new-edges)))))))
                    ;; Cannot delete root.
                    (when (and (not new-state) (eq node trie))
                      (setf new-state (make-empty-node-state)))
                    ;; Try to CAS NODE's state. Restart, if it doesn't
                    ;; work.
                    (unless (or (eq new-state old-state)
                                (eq (sb-ext:compare-and-swap
                                     (node-state node) old-state new-state)
                                    old-state))
                      (go :start))
                    (return-from visit node))))))
       (declare (dynamic-extent #'visit))
       (visit components trie))))

(macrolet
    ((define-scope-trie-function (name parameters &body body)
       (let+ (((&values body declarations documentation)
               (parse-body body :documentation t)))
         `(defun ,name (,@parameters scope trie)
            ,@(when documentation `(,documentation))
            ,@declarations
            (macrolet
                ((visit ((state-var extend? leaf-only?) &body body)
                   (with-unique-names (visit-name)
                     `(flet ((,visit-name (,state-var)
                               ,@body))
                        (declare (dynamic-extent #',visit-name))
                        (scope-trie-%map
                         #',visit-name (scope-components scope) trie
                         ,extend? ,leaf-only?)))))
              ,@body)))))

  (define-scope-trie-function scope-trie-get ()
    "Return two values: 1) nil or the value associated with SCOPE in
     TRIE 2) a Boolean indicating whether a value is associated with
     SCOPE in TRIE."
    (visit (state nil t)
      (return-from scope-trie-get
        (let ((value (node-state-value state)))
          (values value (not (eq value +no-value+)))))))

  (define-scope-trie-function (setf scope-trie-get) (new-value)
    "Set the value associated with SCOPE in TRIE to NEW-VALUE, return
     NEW-VALUE."
    (visit (state t t)
      (make-node-state new-value (node-state-edges state)))
    new-value)

  (define-scope-trie-function scope-trie-rem ()
    "Remove the value associated with SCOPE in TRIE, if any."
    (visit (state nil t)
      (when-let ((old-edges (node-state-edges state)))
        (make-node-state +no-value+ old-edges))))

  (define-scope-trie-function scope-trie-map (function)
    "Call FUNCTION for values associated to super-scopes of SCOPE in
     TRIE."
    (let ((function (coerce function 'function)))
      (visit (state nil nil)
        (let ((value (node-state-value state)))
          (unless (eq value +no-value+)
            (funcall function value))
          state))))

  (define-scope-trie-function scope-trie-update (function)
    "Call FUNCTION with the value associated to SCOPE in TRIE and
     replace that value with the return value of FUNCTION.

     Return two values: 1) nil or the new value associated with SCOPE
     in TRIE 2) a Boolean indicating whether SCOPE is associated with
     a value in TRIE.

     FUNCTION is called with two arguments: 1) nil or the current
     value associated to SCOPE 2) a Boolean indicating whether a value
     is associated to SCOPE in TRIE. FUNCTION should return two
     values: 1) nil or the new value to associate with SCOPE in TRIE
     2) a Boolean indicating whether SCOPE should be associated with a
     value in TRIE.

     FUNCTION is called at least once but may be called multiple times
     in which case the return values of all but the most recent call
     are discarded."
    (let (new new?)
      (visit (state t t)
        (let* ((old-value (node-state-value state))
               (old-edges (node-state-edges state)))
          (setf (values new new?)
                (if (eq old-value +no-value+)
                    (funcall function nil nil)
                    (funcall function old-value t)))
          (cond
            (new?
             (make-node-state new old-edges))
            (old-edges
             (make-node-state +no-value+ old-edges)))))
      (values new new?))))

;;; `sink-scope-trie' data structure

(defstruct (sink-scope-trie
            (:include     node)
            (:constructor make-sink-scope-trie ())
            (:predicate   nil)
            (:copier      nil)))

(declaim (ftype (function (sink-scope-trie scope t) (values list t &optional))
                sink-scope-trie-add sink-scope-trie-remove))

(defun sink-scope-trie-add (trie scope sink)
  (scope-trie-update (lambda (value value?)
                       (declare (ignore value?))
                       (values (list* sink value) t))
                     scope trie))

(defun sink-scope-trie-remove (trie scope sink)
  (scope-trie-update (lambda (value value?)
                       (declare (ignore value?))
                       (let ((new-value (remove sink value :count 1)))
                         (values new-value new-value)))
                     scope trie))

;;; Utilities

(defun print-trie (stream trie &optional colon? at?)
  (declare (ignore colon? at?))
  (print-unreadable-object (trie stream :type t :identity t)))
