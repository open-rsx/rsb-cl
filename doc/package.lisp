;;; package.lisp --- Package definition for the cl-rsb-doc system.
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

(cl:defpackage :rsb.doc
  (:use
   :cl)

  (:export
   :document-system)

  (:documentation
   "This package contains some functions that are used to generate
documentation for the cl-rsb system."))

(cl:in-package :rsb.doc)


;;; Client Interface
;;

(defun document-system ()
  "Generate documentation for the cl-rsb system and related systems."
  (generate-notification-figure))


;;; Internal functions
;;

(defun generate-notification-figure ()
  "Generates a diagram that illustrates the structure of the
notification protocol buffer and related protocol buffers."
  (let* ((system          (asdf:find-system :cl-rsb-and-cl-protobuf))
	 (module          (asdf:find-component system "protocol"))
	 (components      (asdf:module-components module))
	 (files           (map 'list #'asdf:component-pathname
			       components))
	 (descriptor-set  (pbf:load/text files))

	 (figure-pathname (asdf:system-relative-pathname
			   :cl-rsb "doc/figures/protocol"
			   :type "png")))
    (pbb:emit descriptor-set `(:graphviz-image-file
			       :pathname ,figure-pathname))))
