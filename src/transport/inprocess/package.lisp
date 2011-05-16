;;; package.lisp --- Package definition for the transport.inprocess module.
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

(cl:in-package :cl-user)

(defpackage :rsb.transport.inprocess
  (:shadow
   :connector)
  (:use
   :cl
   :alexandria
   :iterate

   :rsb
   :rsb.event-processing
   :rsb.filter
   :rsb.transport)
  (:documentation
   "This package contains a transport that delivers RSB events within
a process."))

(in-package :rsb.transport.inprocess)

(log5:defcategory rsb.transport.inprocess)

(defmacro log1 (category format &rest args)
  `(log5:log-for (or :rsb.transport.inprocess ,(intern (string category) :log5))
		 ,format ,@args))
