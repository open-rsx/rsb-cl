;;; future.lisp --- Unit tests for the future class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.patterns.test)

(deftestsuite future-root (patterns-root)
  ()
  (:documentation
   "Test suite for the future protocol and the `future' class."))

(addtest (future-root
          :documentation
	  "Smoke test for the future class with result retrieval in a
separate thread.")
  smoke/threads

  (ensure-cases (result-args result
		 expected-done expected-error
		 expected-value expected-tag)
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
	((:timeout .04 :error? nil) (:none)         nil     nil nil  :timeout))

    (iter (repeat 10)
	  (bind ((future (make-instance 'future))
		 done error value tag
		 ((:flet make-receiver (args))
		  (bt:make-thread
		   (lambda ()
		     (handler-case
			 (multiple-value-setq (value tag)
			   (apply #'future-result future args))
		       (condition (condition)
			 (setf error condition)))
		     (setf done (future-done? future))))))
	    (make-receiver result-args)
	    (sleep (random .01))
	    (case (first result)
	      (:result (setf (future-result future) (second result)))
	      (:error  (setf (future-error future)  (second result))))
	    (sleep .08)
	    (unless expected-error
	      (ensure-same value expected-value)
	      (ensure-same tag   expected-tag))
	    (when expected-error
	      (ensure (typep error 'condition)))))))

(addtest (future-root
          :documentation
	  "Smoke test for the future class with result retrieval in
the same thread as everything else.")
  smoke/no-threads

  (ensure-cases (result-args result
		 expected-done expected-error
		 expected-value expected-tag)
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
	((:timeout .02 :error? nil) (:none)         nil     nil      nil  :timeout))

    (iter (repeat 10)
	  (let ((future (make-instance 'future)))
	    ;; Maybe set a result or error.
	    (case (first result)
	      (:result (setf (future-result future) (second result)))
	      (:error  (setf (future-error future)  (second result))))
	    ;; Try retrieving the result.
	    (ecase expected-error
	      (:error
	       (ensure-error (apply #'future-result future result-args)))
	      (:timeout
	       (ensure-condition 'bt:timeout
		 (apply #'future-result future result-args)))
	      ((nil)
	       (ensure-same (apply #'future-result future result-args)
			    (values expected-value expected-tag))))))))
