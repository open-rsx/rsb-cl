;;; configurator-client.lisp --- A client that knows its configurator.
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
