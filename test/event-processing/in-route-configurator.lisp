;;; in-route-configurator.lisp --- Unit tests for the in-route-configurator class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(in-package :rsb.event-processing.test)

(defclass mock-connector (rsb.transport:connector
			  broadcast-processor)
  ()
  (:metaclass rsb.transport:connector-class)
  (:wire-type 'string)
  (:direction :in-pull))

(deftestsuite in-route-configurator-root (event-processing-root)
  ((configurator (make-instance 'in-route-configurator
				:scope     "/foo/bar"
				:direction :in-pull)))
  (:function
   (check-configurator (configurator scope direction configurator-filters processor-filters)
     (ensure-same (configurator-scope configurator) (make-scope scope)
		  :test #'scope=)
     (ensure-same (configurator-direction configurator) direction
		  :test #'eq)
     (ensure-same (configurator-filters configurator) configurator-filters
		  :test #'equal)
     (ensure-same (processor-filters
		   (configurator-processor configurator))
		  processor-filters
		  :test #'equal)))
  (:documentation
   "Root test suite for the `in-route-configurator-root' class."))

(addtest (in-route-configurator-root
          :documentation
	  "Test the required state transitions and updates when adding
and removing filters to an `in-route-configurator' instance.")
  adding/removing-filters

  (let ((filter (make-instance 'rsb.filter:conjoin-filter)))
    ;; Adding and removing a filter should not have an effect when
    ;; there are no connectors.
    (notify configurator filter :filter-added)
    (check-configurator configurator "/foo/bar" :in-pull
			(list filter) nil)

    (notify configurator filter :filter-removed)
    (check-configurator configurator "/foo/bar" :in-pull nil nil)

    ;; However, when there is a connector, the filter should get
    ;; propagated to the processor.
    (push (make-instance 'mock-connector)
	  (configurator-connectors configurator))

    (notify configurator filter :filter-added)
    (check-configurator configurator "/foo/bar" :in-pull
			(list filter) (list filter))

    (notify configurator filter :filter-removed)
    (check-configurator configurator "/foo/bar" :in-pull nil nil)))
