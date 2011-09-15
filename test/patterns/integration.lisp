;;; integration.lisp --- Integration test for local and remote servers.
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

(deftestsuite integration-root (patterns-root)
  ((url "inprocess:/rsbtest/server/integration"))
  (:documentation
   "Integration test for `local-server' and `remote-server'
classes."))

(addtest (integration-root
          :documentation
	  "Smoke test for communication between `local-server' and
`remote-server' instances.")
  smoke

  (with-local-server (local-server url)
    (with-methods (local-server)
	(("echo" (arg string)
	    arg))
      (with-remote-server (remote-server url)
	(ensure-random-cases 100 ((s a-string))

	  ;; Invoke with payload result using `call' method
	  (ensure-same (call remote-server "echo" s) s
		       :test #'string=)

	  ;; Invoke with event result using `call' method
	  (call remote-server "echo" s :return :event)

	  ;; Invoke using `funcall'
	  (ensure-same (funcall (server-method remote-server "echo") s) s
		       :test #'string=)

	  ;; Invoke with event result using `funcall'
	  (funcall (server-method remote-server "echo") s
		   :return :event))))))

(addtest (integration-root
          :documentation
	  "Test calling a remote method which signals an error during
execution.")
  error

  (with-local-server (local-server url)
    (with-methods (local-server)
	(("error" (arg string)
	   (error "intentional error")))
      (with-remote-server (remote-server url)

	;; Invoke using `call' method
	(ensure-condition 'remote-method-execution-error
	  (call remote-server "error" "foo"))

	;; Invoke asynchronously using `call' method
	(ensure-same (future-result
		      (call remote-server "error" "foo"
			    :block? nil)
		      :error? nil)
		     (values nil :failed))

	;; Invoke using `funcall'
	(ensure-condition 'remote-method-execution-error
	  (funcall (server-method remote-server "error") "foo"))

	;; Invoke asynchronously using `funcall'
	(ensure-same (future-result
		      (funcall (server-method remote-server "error")
			       "foo"
			       :block? nil)
		      :error? nil)
		     (values nil :failed))))))

(addtest (integration-root
          :documentation
	  "Test timeout behavior when calling non-existent methods.")
  timeout

  (with-remote-server (remote-server url)
    ;; Invoke using `call' method
    (ensure-condition 'bt:timeout
      (future-result (call remote-server "nosuchmethod" "does-not-matter"
			   :block? nil)
		     :timeout .1))
    ;; Invoke using `funcall'
    (ensure-condition 'bt:timeout
      (future-result (funcall (server-method remote-server "nosuchmethod")
			      "does-not-matter"
			      :block? nil)
		     :timeout .1))))
