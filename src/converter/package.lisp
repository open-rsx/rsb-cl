;;; package.lisp --- Package definition for converter module.
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

(cl:defpackage :rsb.converter
  (:nicknames :rsb.conv)
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :more-conditions

   :nibbles
   
   :rsb)

  ;; Conditions
  (:export
   :conversion-error
   :conversion-error-wire-schema

   :wire->domain-conversion-error
   :conversion-error-encoded
   :conversion-error-domain-type

   :domain->wire-conversion-error
   :conversion-error-domain-object
   :conversion-error-wire-type)

  ;; Converter protocol
  (:export
   :domain->wire?
   :wire->domain?

   :domain->wire
   :wire->domain)

  ;; Converter class family
  (:export
   :no-such-converter
   :find-converter-class
   :converter-classes)

  ;; void converter
  (:export
   :+no-value+ ;; marker value
   :no-value   ;; type
   )

  ;; `force-wire-schema' converter class
  (:export
   :force-wire-schema
   :converter-wire-schema)

  (:documentation
   "This package contains mechanisms for converting between domain
object (which are Lisp object) and data representation in different
kinds of wire formats."))
