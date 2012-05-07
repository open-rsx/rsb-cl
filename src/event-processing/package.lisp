;;; package.lisp --- Package definition for event-processing module.
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

(cl:defpackage :rsb.event-processing
  (:nicknames
   :rsb.ep)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions
   
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

  ;; Configurator protocol
  (:export
   :make-processor
   :collect-processor-mixins)

  ;; `configurator' class
  (:export
   :configurator
   :configurator-scope
   :configurator-direction
   :configurator-processor
   :configurator-connectors)

  ;; `in-route-configurator' class
  (:export
   :in-route-configurator
   :configurator-filters)

  ;; `out-route-configurator' class
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

  ;; `error-policy-mixin' class
  (:export
   :error-policy-mixin
   :processor-error-policy
   :apply-error-policy
   :invoke-with-error-policy

   :with-error-policy)

  ;; `error-handling-dispatcher-mixin' class
  (:export
   :error-handling-dispatcher-mixin)

  ;; `client' class and protocol
  (:export
   :client
   :client-configurator)

  ;; Exported for unit test
  (:export
   :merge-implementation-infos)

  (:documentation
   "This package contains protocol, classes and methods for routing
and processing of events."))
