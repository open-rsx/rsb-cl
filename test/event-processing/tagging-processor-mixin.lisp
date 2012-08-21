;;; tagging-processor-mixin.lisp ---
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

(cl:in-package :rsb.event-processing.test)


;;; Mock processor class
;;

(defclass mock-tagging-processor (broadcast-processor
				  tagging-processor-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))


;;; Tests
;;

(deftestsuite tagging-processor-mixin-root (event-processing-root)
  ((simple-processor (make-instance 'mock-tagging-processor)))
  (:documentation
   "TODO(jmoringe): document"))

(addtest (tagging-processor-mixin-root
          :documentation
	  "Smoke test for the `tagging-processor-mixin' class.")
  smoke

  (let ((result))
    (setf (getf (processor-tags simple-processor) :tag) "foo"
	  (processor-tag? simple-processor) t)

    (push #'(lambda (event) (push event result))
	  (handlers simple-processor))
    (handle simple-processor (make-event "/" "bla"))
    (setf rsb::*proc* simple-processor)
    (setf rsb::*event* (first result))
    (ensure-same (meta-data (first result) :tag) "foo"
		 :test #'string=)))
