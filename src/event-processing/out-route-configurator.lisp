;;; out-route-configurator.lisp --- Configurator class for out-direction processing.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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
