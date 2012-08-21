;;; transport-filtering-configurator-mixin.lisp --- Mixin for transport tag filtering configurators.
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

(cl:in-package :rsb.event-processing)

(defclass transport-filtering-configurator-mixin ()
  ((transport-tag-filter :type     transport-tag-filter
			 :reader   configurator-transport-tag-filter
			 :initform (make-instance 'transport-tag-filter)
			 :documentation
			 "Stores the `transport-tag-filter' instance
that the configurator uses to communicate transport rankings to its
associated connectors."))
  (:documentation
   "This class is intended to be mixed into configurator classes that
filter processed events based on transport list tags when multiple
connectors receive events."))

(defmethod (setf configurator-connectors) :around
    ((new-value    list)
     (configurator transport-filtering-configurator-mixin))
  "Track the number of connectors attached to CONFIGURATOR. On
transitions from none or one to multiple connectors or vice versa,
enable or disable the filtering of events based on transport list
tags."
  (let+ (((&accessors
	   (connectors configurator-connectors)
	   (filter     configurator-transport-tag-filter)) configurator)
	 (old-num-connectors (length connectors))
	 (result             (call-next-method))
	 (new-num-connectors (length connectors)))
    (cond
      ;; If we had zero or one connectors before and now have more
      ;; than one, enable filtering of events based on transport list
      ;; tags.
      ((and (<= old-num-connectors 1) (> new-num-connectors 1))
       (let ((ranking (compute-transport-ranking connectors)))
	 (log1 :info configurator "Now > 1 in-direction connectors; Transport filtering with ~{~S~^ ⋨ ~}."
	       ranking)
	 (setf (filter-transport-ranking filter) ranking)
	 (notify configurator filter :filter-added)))

      ;; If we had multiple connectors before and now have one or
      ;; zero, disable filtering of events based on transport list
      ;; tags.
      ((and (> old-num-connectors 1) (<= new-num-connectors 1))
       (log1 :info configurator "Now <= 1 in-direction connectors; Disabling transport filtering.")
       (notify configurator filter :filter-removed)))

    result))


;;; Utility functions
;;

(defun compute-transport-ranking (connectors)
  "Compute and return a list of transport schemas based on the
priority settings associated with CONNECTORS."
  (map 'list (compose #'first
		      (find-symbol "CONNECTOR-SCHEMAS" :rsb.transport)) ;; TODO(jmoringe):
       (sort (copy-list connectors) #'<
	     :key (find-symbol "CONNECTOR-PRIORITY" :rsb.transport)))) ;; TODO(jmoringe):

;; Local Variables:
;; coding: utf-8
;; End:
