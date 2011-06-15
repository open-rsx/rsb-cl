;;; package.lisp --- Package definition for event-processing module.
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

(in-package :cl-user)

(defpackage :rsb.event-processing
  (:nicknames
   :rsb.ep)

  (:use
   :cl
   :alexandria
   :iterate
   :bind

   :rsb)

  ;; Push source protocol
  (:export
   :handlers)

  ;; Pull source protocol
  (:export
   :emit)

  ;; Sink protocol
  (:export
   :handle)

  ;; Dispatching processor protocol
  (:export
   :dispatch)

  ;; Notification protocol
  (:export
   :notify)

  ;; Processor class family
  (:export
   :make-processor-class
   :ensure-processor-class
   :clear-processor-classes
   :processor-classes)

  ;;
  (:export
   :configurator
   :configurator-direction
   :configurator-processor
   :configurator-connectors)

  ;;
  (:export
   :in-route-configurator
   :configurator-filters)

  ;;
  (:export
   :out-route-configurator)

  ;; `broadcast-processor' class
  (:export
   :broadcast-processor)

  ;; `filtering-processor-mixin' class
  (:export
   :filtering-processor-mixin
   :processor-filters)

  ;; `deliver-timestamp-mixin' class
  (:export
   :deliver-timestamp-mixin)

  ;; `client' class and protocol
  (:export
   :client
   :client-configurator)

  (:documentation
   "This package contains protocol, classes and methods for routing
and processing of events."))


(in-package :rsb.event-processing)

(log5:defcategory :rsb.event-processing)

(defmacro log1 (category format &rest args)
  `(log5:log-for (or :rsb.event-processing ,(intern (string category) :log5))
		 ,format ,@args))
