;;; integration.lisp --- Integration test for local and remote servers.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.patterns.test)

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
