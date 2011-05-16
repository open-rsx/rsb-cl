;;; protocol.lisp ---
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


;;; Push source protocol
;;

(defgeneric handlers (source)
  (:documentation
   "Return the handlers associated to SOURCE."))

(defgeneric (setf handlers) (new-value source)
  (:documentation
   "Set the list of handlers associated to SOURCE to NEW-VALUE."))


;;; Pull source protocol
;;

(defgeneric emit (source block?)
  (:documentation
   "Block until an event is available from SOURCE. Return it."))


;;; Sink protocol
;;

(defgeneric handle (sink data)
  (:documentation
   "Put DATA into SINK. SINK may for example process, relay or discard
DATA."))


;;; Default behavior
;;

(defmethod handle ((sink function)
		   (data t))
  "If SINK is a function, call it with DATA."
  (funcall sink data))

(defmethod handle ((sink list)
		   (data t))
  "If SINK is a list, treat it as a list of sinks and let each
contained sink handle DATA."
  (map nil (rcurry #'handle data) sink))
