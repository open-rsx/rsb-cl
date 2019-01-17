;;;; inference.lisp --- Tests for the inference functions provided by the model.inference module.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.inference.test)

;;; Utilities

(defun make-participant-info (kind scope transports)
  (make-instance 'participant-info
                 :kind       kind
                 :id         (uuid:make-v1-uuid)
                 :scope      scope
                 :transports (mapcar #'puri:uri transports)
                 :type       t))

(defun make-participant-node (info &rest children)
  (make-instance 'basic-participant-node
                 :info     (apply #'make-participant-info info)
                 :children (mapcar (curry #'apply #'make-participant-node)
                                   children)))

(defun+ check-communication?-cases
    (from to (expected-result expected-definitive? expected-uris))
  (let+ ((expected-uris (mapcar (lambda+ ((left . right))
                                  (cons (puri:uri left) (puri:uri right)))
                                expected-uris))
         ((&values result definitive? uris)
          (communication? from to))
         ((&flet uri-pairs-set-equal (left right)
            (set-equal left right
                       :test (lambda+ ((from-left  . to-left)
                                       (from-right . to-right))
                               (and (puri:uri= from-left from-right)
                                    (puri:uri= to-left   to-right)))))))
    (is (eql                 expected-result      result))
    (is (eql                 expected-definitive? definitive?))
    (is (uri-pairs-set-equal expected-uris        uris))))

;;; `communication?'

(def-suite* rsb-model-inference-communication?-root
  :in rsb-model-inference-root
  :description
  "Root unit test suite for the `communication?' generic function.")

(test smoke/scope+uri
  "Smoke test for the `communication?' generic function focusing on
   scope and URI inputs."

  (mapc
   (lambda+ ((from to expected))
     (check-communication?-cases from to expected))

   `(;; Scopes
     (,(make-scope "/foo")      ,(make-scope "/foo")               (t   t   ()))
     (,(make-scope "/foo")      ,(make-scope "/bar")               (nil t   ()))
     (,(make-scope "/foo")      ,(make-scope "/foo/bar")           (nil nil ()))

     ;; URIs
     (,(puri:uri "socket:/foo") ,(puri:uri "socket:/foo")          (t   t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "socket:/bar")          (nil t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "socket:/foo/bar")      (nil nil ()))

     (,(puri:uri "socket:/foo") ,(puri:uri "socket://baz/foo")     (nil t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "socket://baz/bar")     (nil t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "socket://baz/foo/bar") (nil t   ()))

     (,(puri:uri "socket:/foo") ,(puri:uri "spread:/foo")          (nil t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "spread:/bar")          (nil t   ()))
     (,(puri:uri "socket:/foo") ,(puri:uri "spread:/foo/bar")      (nil t   ()))

     ;; Mixed
     (,(puri:uri "/foo")        ,(make-scope "/foo")               (nil nil ()))
     (,(puri:uri "/foo")        ,(make-scope "/bar")               (nil t   ()))
     (,(puri:uri "/foo")        ,(make-scope "/foo/bar")           (nil nil ()))
     (,(make-scope "/foo")      ,(puri:uri "/foo")                 (nil nil ()))
     (,(make-scope "/foo")      ,(puri:uri "/bar")                 (nil t   ()))
     (,(make-scope "/foo")      ,(puri:uri "/foo/bar")             (nil nil ())))))

(test smoke/participant-info
  "Smoke test for the `communication?' generic function focusing on
   `participant-info' inputs."

  (mapc
   (lambda+ ((from to expected))
     (let ((from (apply #'make-participant-info from))
           (to   (apply #'make-participant-info to)))
       (check-communication?-cases from to expected)))

   `(;; Kinds that do not communicate.
     ((:listener "/foo" ("socket:")) (:listener "/foo" ("socket:"))
      (nil t ()))
     ((:informer "/foo" ("socket:")) (:informer "/foo" ("socket:"))
      (nil t ()))
     ((:listener "/foo" ("socket:")) (:informer "/foo" ("socket:"))
      (nil t ()))

     ;; informer -> listener
     ((:informer "/foo" ("socket:")) (:listener "/foo" ("spread:"))
      (nil t ()))
     ((:informer "/foo" ("socket:")) (:listener "/bar" ("socket:"))
      (nil t ()))
     ((:informer "/foo" ("socket:")) (:listener "/foo/bar" ("socket:"))
      (nil nil (("socket:" . "socket:"))))
     ((:informer "/foo" ("socket:")) (:listener "/foo" ("socket:"))
      (t t (("socket:" . "socket:"))))

     ;; remote-method <-> local-method
     ((:remote-method "/foo" ("socket:")) (:local-method  "/bar" ("socket:"))
      (nil t ()))
     ((:remote-method "/foo" ("socket:")) (:local-method  "/foo/bar" ("socket:"))
      (nil t ()))
     ((:remote-method "/foo" ("socket:")) (:local-method  "/foo" ("socket:"))
      (t t (("socket:" . "socket:"))))

     ((:local-method "/foo" ("socket:")) (:remote-method  "/bar" ("socket:"))
      (nil t ()))
     ((:local-method "/foo" ("socket:")) (:remote-method  "/foo/bar" ("socket:"))
      (nil t ()))
     ((:local-method "/foo" ("socket:")) (:remote-method  "/foo" ("socket:"))
      (t t (("socket:" . "socket:"))))

     ;; Multiple transports
     ((:informer "/foo" ("socket:" "spread:" "inprocess:"))
      (:listener "/foo" ("socket://localhost:40" "spread:" "inprocess:"))
      (t t (("spread:" . "spread:") ("inprocess:" . "inprocess:"))))

     ;; No transports.
     ((:informer "/foo" ()) (:listener "/foo" ()) (t t ())))))

(test smoke/nodes
  "Smoke test for the `communication?' generic function focusing on
   node inputs."

  (mapc
   (lambda+ ((from to expected))
     (let ((from (apply #'make-participant-node from))
           (to   (apply #'make-participant-node to)))
       (check-communication?-cases from to expected)))

   '(;; Unknown participant kind.
     (((:some-participant "/foo" ("socket:")))
      ((:some-participant "/bar" ("socket:")))
      (nil nil ()))
     ;; Unknown participant kind with children.
     (((:some-participant "/foo" ("socket:"))
       ((:informer "/foo" ("socket:"))))
      ((:some-participant "/bar" ("socket:"))
       ((:listener "/foo" ("socket:"))))
      (t t (("socket:" . "socket:"))))
     ;; Same with multiple children and transports.
     (((:some-participant "/foo" ("socket:" "spread:"))
       ((:informer "/foo" ("socket:" "spread:")))
       ((:informer "/bar" ("socket:" "spread:"))))
      ((:some-participant "/bar" ("spread:" "socket:"))
       ((:listener "/foo"     ("spread:" "socket:")))
       ((:listener "/bar/baz" ("spread:" "socket:"))))
      (t t (("socket:" . "socket:") ("spread:" . "spread:"))))
     ;; Local- and remote-server.
     (((:remote-server "/foo" ("socket:"))
       ((:remote-method "/foo/bar" ("socket:"))))
      ((:local-server "/foo" ("socket:"))
       ((:local-method "/foo/bar" ("socket:"))))
      (t t (("socket:" . "socket:")))))))
