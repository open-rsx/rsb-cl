;;; transport-tag-configurator-mixin.lisp --- Mixin class for attaching transport lists to events.
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

(defclass transport-tagging-configurator-mixin (tagging-configurator-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into configurator classes that
attach a list of transports to processed events when multiple
connectors handle events."))

(defmethod (setf configurator-connectors) :around
    ((new-value    list)
     (configurator transport-tagging-configurator-mixin))
  "Track the number of connectors attached to CONFIGURATOR. On
transitions from none or one to multiple connectors or vice versa,
enable or disable the tagging of events with a list of transports."
  (let+ (((&accessors (connectors configurator-connectors)) configurator)
	 (old-num-connectors (length connectors))
	 (result             (call-next-method))
	 (new-num-connectors (length connectors)))
    (cond
      ;; If we had zero or one connectors before and now have more
      ;; than one, enable attaching transport lists to events.
      ((and (<= old-num-connectors 1) (> new-num-connectors 1))
       (let ((tag (compute-transport-tag connectors)))
	 (log1 :info configurator "Now > 1 out-direction connectors; Transport tagging with ~S."
	       tag)
	(setf (getf (configurator-tags configurator) :transports) tag)))

      ;; If we had multiple connectors before and now have one or
      ;; zero, disable attaching transport lists to events.
      ((and (> old-num-connectors 1) (<= new-num-connectors 1))
       (log1 :info configurator "Now <= 1 out-direction connectors; Disabling transport tagging.")
       (remove-from-plistf (configurator-tags configurator) :transports)))

    result))


;;; Utility functions
;;

(defun compute-transport-tag (connectors)
  "Compute and return a transport tag string for CONNECTORS."
  (let ((schemas (map 'list (compose #'first
				     (find-symbol "CONNECTOR-SCHEMAS" :rsb.transport)) ;;; TODO(jmoringe):
		      connectors)))
   (format nil "~{~(~A~)~^;~}" schemas)))
