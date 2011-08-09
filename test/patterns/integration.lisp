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
	  (ensure-same (call remote-server "echo" s) s
		       :test #'string=))))))

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
	(ensure-condition 'remote-method-execution-error
	  (call remote-server "error" "foo"))))))

(addtest (integration-root
          :documentation
	  "Test timeout behavior when calling non-existent methods.")
  timeout

  (with-remote-server (remote-server url)
    (ensure-condition 'bt:timeout
      (future-result (call remote-server "no-such-method" "does-not-matter"
			   :block? nil)
		     :timeout .1))))
