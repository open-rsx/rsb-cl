;;; configurator-client.lisp --- A client that knows its configurator.
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

(defclass client ()
  ((configurator :initarg  :configurator
		 :type     configurator
		 :reader   client-configurator
		 :documentation
		 "The configurator instance working for this
client."))
  (:default-initargs
   :configurator (missing-required-initarg 'client :configurator))
  (:documentation
   "Instance of this of this class are clients of the event processing
subsystem in the sense that they have an associated `configurator'."))

(defmethod transport-specific-urls ((component client))
  "Return transport URL for connectors used by COMPONENT."
  (iter (for connector in (configurator-connectors
			   (client-configurator component)))
	(collect
	    (funcall (find-symbol "CONNECTOR-RELATIVE-URL" :rsb.transport)
		     connector component))))
