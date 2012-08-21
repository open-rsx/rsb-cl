;;; tagging-configurator-mixin.lisp --- A mixin for event tagging configurators.
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

(defclass tagging-configurator-mixin ()
  ()
  (:documentation
   "This class is intended to mixed into configurator classes that
configure an event processor to tag processed events in some way. As a
result, the respectively used processor class has to support tagging
via the `processor-tags' and `processor-tag?' protocol methods."))

(defmethod shared-initialize :after ((instance   tagging-configurator-mixin)
                                     (slot-names t)
                                     &key
				     tags)
  (setf (configurator-tags instance) tags))

(defmethod collect-processor-mixins append ((configurator tagging-configurator-mixin))
  '(tagging-processor-mixin))

(defmethod configurator-tags ((configurator tagging-configurator-mixin))
  "Return the plist of tags currently applied by the processor of
CONFIGURATOR."
  (processor-tags (configurator-processor configurator)))

(defmethod (setf configurator-tags) ((new-value    list)
				     (configurator tagging-configurator-mixin))
  "Set the tags applied to processed events by the processor of
CONFIGURATOR to NEW-VALUE. Tagging is turned on or off (via
`processor-tag?') in the processor when NEW-VALUE is non-nil or nil
respectively."
  (let+ (((&accessors (tags processor-tags)
		      (tag? processor-tag?))
	  (configurator-processor configurator)))
    (setf tags new-value
	  tag? (when new-value t))))
