;;; package.lisp --- Package definition for serialization module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:defpackage :rsb.serialization
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :more-conditions

   :nibbles

   :rsb
   :rsb.converter)

  (:export
   :notification->event
   :event->notification)

  (:export
   :no-such-serialization-class
   :find-serialization-class
   :serializtion-classes)

  ;; Utility functions
  (:export
   :unix/microseconds->timestamp
   :timestamp->unix/microseconds

   :bytes->wire-schema
   :wire-schema->bytes)

  ;; Optimization settings
  (:import-from :cl-rsb-system
   :+optimization-fast+unsafe+)

  (:documentation
   "This package contains functions and classes which implement
the (de)serialization of events to/from different kinds of
notifications.

The primay interface for this event (de)serialization are the generic
functions

notification->event                 [generic function]

  Deserialize a notification into an event.

event->notification                 [generic function]

  Serialize an event into a notification.

The package also contains utility functions for:

* conversion of timestamps
* (de)serialization of wire-schemas"))
