;;; out-route-configurator.lisp --- Configurator class for out-direction processing.
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

(in-package :rsb.event-processing)

(defclass out-route-configurator (configurator)
  ()
  (:default-initargs
   :direction :out)
  (:documentation
   "Instances of this class configure out-direction connectors and an
event processor for sending of events."))

(defmethod collect-processor-mixins append ((configurator out-route-configurator))
  '(broadcast-processor))


;;; Connectors
;;

(defmethod notify ((configurator out-route-configurator)
		   (connector    t)
		   (action       (eql :connector-added)))
  (bind (((:accessors-r/o (processor configurator-processor)) configurator))
    (call-next-method)
    (log1 :trace configurator "Connecting ~S -> ~S" processor connector)
    (push connector (handlers processor))))

(defmethod notify ((configurator out-route-configurator)
		   (connector    t)
		   (action       (eql :connector-removed)))
  (bind (((:accessors-r/o (processor configurator-processor)) configurator))
    (log1 :trace configurator "Disconnecting ~S -> ~S" processor connector)
    (removef (handlers processor) connector)
    (call-next-method)))
