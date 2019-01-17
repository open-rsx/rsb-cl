;;;; future.lisp --- Unit tests for the future class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(def-suite* future-root
  :in patterns-request-reply-root
  :description
  "Test suite for the future protocol and the `future' class.")

(test smoke/threads
  "Smoke test for the future class with result retrieval in a separate
   thread."

  (mapc
   (lambda+ ((result-args result
              expected-done expected-error
              expected-value expected-tag))
     (iter (repeat 10)
           (let+ ((future (make-instance 'future))
                  done error value tag
                  ((&flet make-receiver (args)
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (multiple-value-setq (value tag)
                              (apply #'future-result future args))
                          (condition (condition)
                            (setf error condition)))
                        (setf done (future-done? future)))))))
             (make-receiver result-args)
             (sleep (random .01))
             (case (first result)
               (:result (setf (future-result future) (second result)))
               (:error  (setf (future-error future)  (second result))))
             (sleep .08)
             (is (eq expected-done done))
             (unless expected-error
               (is (eq expected-value value))
               (is (eq expected-tag   tag)))
             (when expected-error
               (is (typep error 'condition))))))

   '(;; Without timeout and with    error signaling
     (()                         (:result :foo)  :done   nil :foo :done)
     (()                         (:error  "bla") :failed t   nil  nil)
     ;; Without timeout and without error signaling
     ((:error? nil)              (:result :foo)  :done   nil :foo :done)
     ((:error? nil)              (:error  "bla") :failed nil nil  :failed)
     ;; With    timeout and with    error signaling
     ((:timeout .04)             (:result :foo)  :done   nil :foo :done)
     ((:timeout .04)             (:error  "bla") :failed t   nil  nil)
     ((:timeout .04)             (:none)         nil     t   nil  nil)
     ;; With    timeout and without error signaling
     ((:timeout .04 :error? nil) (:result :foo)  :done   nil :foo :done)
     ((:timeout .04 :error? nil) (:error  "bla") :failed nil nil  :failed)
     ((:timeout .04 :error? nil) (:none)         nil     nil nil  :timeout))))

(test smoke/no-threads
  "Smoke test for the future class with result retrieval in the same
   thread as everything else."

  (mapc
   (lambda+ ((result-args result
              expected-done expected-error
              expected-value expected-tag))
     (iter (repeat 10)
           (let+ ((future (make-instance 'future))
                  ((&flet do-it ()
                     (apply #'future-result future result-args))))
             ;; Maybe set a result or error.
             (case (first result)
               (:result (setf (future-result future) (second result)))
               (:error  (setf (future-error future)  (second result))))
             ;; Check done state.
             (is (eq expected-done (future-done? future)))
             ;; Try retrieving the result.
             (ecase expected-error
               (:error
                (signals error (do-it)))
               (:timeout
                (signals bt:timeout (do-it)))
               ((nil)
                (is (equal (list expected-value expected-tag)
                           (multiple-value-list (do-it)))))))))

   '(;; Without timeout and with    error signaling
     (()                         (:result :foo)  :done   nil      :foo :done)
     (()                         (:error  "bla") :failed :error   nil  nil)
     ;; Without timeout and without error signaling
     ((:error? nil)              (:result :foo)  :done   nil      :foo :done)
     ((:error? nil)              (:error  "bla") :failed nil      nil  :failed)
     ;; With    timeout and with    error signaling
     ((:timeout .02)             (:result :foo)  :done   nil      :foo :done)
     ((:timeout .02)             (:error  "bla") :failed :error   nil  nil)
     ((:timeout .02)             (:none)         nil     :timeout nil  nil)
     ;; With    timeout and without error signaling
     ((:timeout .02 :error? nil) (:result :foo)  :done   nil      :foo :done)
     ((:timeout .02 :error? nil) (:error  "bla") :failed nil      nil  :failed)
     ((:timeout .02 :error? nil) (:none)         nil     nil      nil  :timeout))))
