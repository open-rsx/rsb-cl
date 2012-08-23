;;; connectors.lisp --- Unit tests for connector classes.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.transport.spread.test)


;;; `in-connector' superclass
;;

(deftestsuite in-connector-root (transport-spread-root
				 connector-suite)
  ((a-string           (sb-ext:string-to-octets "foobarbaz"))
   (empty-notification (pb:pack* (make-instance
				  'rsb.protocol:notification))))
  (:documentation
   "Tests for the `in-connector' class and associated methods."))

(addtest (in-connector-root
          :documentation
	  "Test `message->event' method.")
  message->event

  (ensure-cases (notification wire-schema connector-args condition?)
      `(;; In these cases, protocol buffer unpacking fails
	(,a-string           :foo (:error-policy nil)          t)
	(,a-string           :foo (:error-policy ,#'continue)  nil)
	(,a-string           :foo (:error-policy ,#'log-error) nil)

	;; Protocol buffer unpacking succeeds, but conversion to event
	;; fails.
	(,empty-notification :foo (:error-policy nil)          t)
	(,empty-notification :foo (:error-policy ,#'continue)  nil)
	(,empty-notification :foo (:error-policy ,#'log-error) nil))

    (let ((connector (apply #'make-instance 'in-pull-connector ;;; TODO(jmoringe): class
			    (append common-args connector-args))))
      (if condition?
	  (ensure-condition 'decoding-error
	    (rsb.ep:with-error-policy (connector)
	      (message->event connector notification wire-schema)))
	  (ensure-null
	   (rsb.ep:with-error-policy (connector)
	     (message->event connector notification wire-schema)))))))


;;; Connector classes
;;

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.spread "~A-CONNECTOR" direction))
	     (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction)))
	 `(progn
	    (deftestsuite ,suite-name (transport-spread-root
				       connector-suite)
	      ()
	      (:documentation
	       ,(format nil "Test suite for the `~(~A~)' class."
			class-name)))

	    (define-basic-connector-test-cases ,class-name
	      :initargs           common-args
	      :expected-direction ,(make-keyword direction)
	      :expected-wire-type 'octet-vector
	      :expected-schemas   '(:spread))

	    (addtest (,suite-name
		      :documentation
		      ,(format nil "Test constructing `~(~A~)' instances."
			       class-name))
	      construct/invalid

	      ;; Missing :host, :port, :name or :connection initarg.
	      (ensure-condition 'missing-required-initarg
		(make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
