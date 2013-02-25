;;; processor-mixins.lisp --- Unit tests for processor mixin classes.
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package :rsb.event-processing.test)

(deftestsuite  rsb.event-processing.transform-mixin-root (event-processing-root)
  ()
  (:documentation
   "Unit tests for the `transform-mixin' processor mixin class.

See test suite for `transform!' generic function."))

(defclass transform-mock-processor (transform-mixin
				    mock-processor)
  ())

(addtest (rsb.event-processing.transform-mixin-root
          :documentation
	  "Smoke test for the `transform-mixin' processor mixin
class.")
  smoke

  (ensure-cases (initargs objects expected)
      `(;; Some invalid transforms.
	((:transform :no-such-transform)     (:does-not-matter) transform-error)
	((:transform (:no-such-transform))   (:does-not-matter) transform-error)
	((:transform ,#'1+)                  (:wrong-type)      transform-error)

	;; These are valid
	((:transform ,#'1+)                  (1 2 3)            (2 3 4))
	((:transform (,#'1+ ,(curry #'* 2))) (1 2 3)            (3 5 7))
	((:transform (,(curry #'* 2) ,#'1+)) (1 2 3)            (4 6 8)))

    (let+ (((&flet do-it ()
	      (let ((processor (apply #'make-instance
				      'transform-mock-processor initargs)))
		(mapc (curry #'handle processor) objects)
		(processor-handled processor)))))
      (case expected
	(transform-error (ensure-condition 'transform-error (do-it)))
	(t               (ensure-same (do-it) expected :test #'equal))))))
