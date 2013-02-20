;;; listener.lisp --- Unit tests for listener.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsb.test)

(deftestsuite listener-root (root
			     participant-suite)
  ()
  (:function
   (send-some (informer)
     (iter (repeat 100)
	   (send informer "foo")
	   (send informer "bar"))))
  (:documentation
   "Unit tests for the `listener' class and `make-listener'
function."))

(define-basic-participant-test-cases :listener
  '("/listener/construction"
    nil
    "/listener/construction")
  '("/listener/construction"
    (:transports ((:inprocess &inherit)))
    "/listener/construction")
  '("/listener/construction"
    (:converters ((t . :foo)))
    "/listener/construction")
  '("inprocess://localhost/listener/construction"
    nil
    "/listener/construction")

  ;; No transports => error
  '("/listener/construction"
    (:transports nil)
    :error))

(addtest (listener-root
          :documentation
	  "Test adding and removing handlers to/from a `listener'
instance.")
  handlers

  (with-listener (listener "/foo")
    ;; Initially, there should not be any handlers.
    (ensure-null (rsb.ep:handlers listener))

    ;; Test adding and removing a handler.
    (let ((handler #'(lambda (event) (declare (ignore event)))))
      (push handler (rsb.ep:handlers listener))
      (ensure-same (rsb.ep:handlers listener) (list handler)
		   :test #'equal)

      (removef (rsb.ep:handlers listener) handler)
      (ensure-null (rsb.ep:handlers listener)))))

(addtest (listener-root
          :documentation
	  "Test receiving data sent by an informer.")
  receive

  (with-informer (informer "/foo" t)
    (with-listener (listener "/foo")
      ;; Test receive
      (send-some informer)

      ;; Test receive with filters
      (push (filter :origin :origin (uuid:make-v1-uuid))
	    (receiver-filters listener))
      (send-some informer)

      ;; Test receive with handlers
      (push #'(lambda (event) (declare (ignore event)))
	    (rsb.ep:handlers listener))
      (send-some informer))))

(define-error-hook-test-case (listener)
  ;; Force an error during dispatch by injecting a signaling
  ;; pseudo-filter.
  (push (lambda (event)
	  (let ((error (make-condition 'simple-error
				       :format-control   "I hate ~A"
				       :format-arguments (list event))))
	    (push error expected-errors)
	    (error error)))
	(receiver-filters listener))

  ;; Try to send and receive an event to trigger the error.
  (send informer "foo"))
