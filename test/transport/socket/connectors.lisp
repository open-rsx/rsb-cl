;;; connectors.lisp --- Unit tests for connector classes.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.transport.socket.test)


;;; Connector classes
;;

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "~A-CONNECTOR" direction))
	     (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction)))
	`(progn
	   (deftestsuite ,suite-name (transport-socket-root
				      connector-suite)
	     ()
	     (:documentation
	      ,(format nil "Test suite for the `~(~A~)' class."
		       class-name)))

	   (define-basic-connector-test-cases ,class-name
	     :expected-direction ,(make-keyword direction)
	     :expected-wire-type 'octet-vector
	     :expected-schemas   '(:socket)
	     :construct-args     (:host      "localhost"
				  :port      *next-port*
				  :converter :fundamental-null))

	   (addtest (,suite-name
		     :documentation
		     ,(format nil "Test constructing `~(~A~)' instances."
			      class-name))
	     construct/invalid

	     ;; Missing :converter initarg.
	     (ensure-condition 'missing-required-initarg
	       (make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
