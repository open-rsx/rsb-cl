;;; expose-wire-schema-mixin.lisp --- Mixin for connectors that expose a wire-schema
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

(cl:in-package :rsb.transport)

(defclass expose-wire-schema-mixin ()
  ((expose-wire-schema? :initarg  :expose-wire-schema?
			:initarg  :expose-wire-schema ;; for option
			:type     boolean
			:reader   connector-expose-wire-schema?
			:initform nil
			:documentation
			"Controls whether the connector should expose
the wire-schemas of received notifications in events constructed form
these notifications."))
  (:metaclass connector-class)
  (:options
   (:expose-wire-schema &slot))
  (:documentation
   "This class is intended to be mixed into connector classes that
should be able to store the wire-schema of received notifications in
the events constructed from the notifications."))
