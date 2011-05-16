;;; processor.lisp ---
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

(defclass processor () ;; TODO necessary?
  ()
  (:documentation
   "DOC"))

(defclass broadcast-processor (processor) ;; TODO -mixin?
  ((handlers :initarg  :handlers
	     :type     list
	     :accessor handlers
	     :initform nil
	     :documentation
	     ""))
  (:documentation
   "DOC"))

(defmethod handle ((processor broadcast-processor)
		   (event     event))
  "DOC"
  (dispatch processor event))

(defmethod dispatch ((processor broadcast-processor)
		     (event     event))
  "Dispatch EVENT to handlers of PROCESSOR."
  (handle (handlers processor) event))

(defclass in-processor (processor) ;; TODO necessary?
  ()
  (:documentation
   "DOC"))

(defclass out-processor (processor) ;; TODO necessary?
  ()
  (:documentation
   "DOC"))
