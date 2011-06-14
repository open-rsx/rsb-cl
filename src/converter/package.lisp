;;; package.lisp --- Package definition for converter module.
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

(defpackage :rsb.converter
  (:nicknames :rsb.conv)
  (:use
   :cl
   :alexandria
   :bind
   :iterate

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
  (:documentation
   "This package contains mechanisms for converting between domain
object (which are Lisp object) and data representation in different
kinds of wire formats."))

(in-package :rsb.converter)


;;; Logging
;;

(log5:defcategory rsb.converter)

(defmacro log1 (category format &rest args)
  `(log5:log-for (or :rsb.converter ,(intern (string category) :log5))
		 ,format ,@args))
