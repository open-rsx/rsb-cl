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

(in-package :rsb.transport)


;;; Connector protocol
;;

(defgeneric connector-url (connector)
  (:documentation
   "Return a base URL that can be used to locate resources via
CONNECTOR."))

(defgeneric connector-relative-url (connector thing)
  (:documentation
   "Return a complete URL suitable for locating the resource THING via
 CONNECTOR."))

;; TODO the symbol is in package rsb; move this generic to some file in src?
(defgeneric notify (connector thing action) ;; or maybe part of the filter protocol?
  (:documentation
   "Methods should one of the symbols :not-implemented or :implemented
to indicate whether the combination of THING and ACTION could be
implemented by CONNECTOR on the transport level."))


;;; Default behavior
;;

(defmethod notify ((connector t) (filter t) (action t))
  "The default behavior is to do nothing and state the fact."
  :not-implemented)

;; TODO is this the right place to implement this behavior?
(defmethod notify ((connector t) (filter rsb::listener) (action t))
  "The default behavior is to produce notifications for all filters
contained in the subscription FILTER and merge the resulting
implementation infos."
  (or (when (eq (call-next-method) :implemented)
	:implemented)
      ;; The result is :not-implemented unless every single filter has
      ;; successfully been implemented
      (reduce #'rsb.event-processing::merge-implementation-infos ;; TODO
	      (map 'list #'(lambda (filter) (notify connector filter action))
		   (rsb::listener-filters filter))
	      :initial-value :implemented)))


;;; Transport implementations
;;

(dynamic-classes:define-findable-class-family transport
    "Transports are implemented by input and output connector
classes. These are designated by names of the form :TRANSPORT-in
and :TRANSPORT-out respectively.")

;; TODO add a type argument here? the connector instance would then
;; produce/consume events with payloads of the specified type (by
;; internally managing the required converter)
(defun make-connector (name direction &rest args)
  "DOC"
  (check-type name      keyword   "a keyword")
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (let ((name1 (intern (concatenate
			'string (string name) "-" (string direction))
		       :keyword)))
    (apply #'make-instance (find-transport-class name1) args)))


(defun make-connectors (specs direction)
  "TODO(jmoringe): document"
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (iter (for (name . args) in specs)
	(collect (apply #'make-connector name direction args))))
