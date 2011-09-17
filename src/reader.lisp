;;; reader.lisp --- Pull-based receiving participant class.
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

(in-package :rsb)

(defclass reader (receiving-client)
  ()
  (:documentation
   "Instances of this class provide a pull-based interface for
receiving events."))

(defmethod receive ((reader reader)
		    &key
		    (block? t))
  (let ((processor (rsb.ep:configurator-processor
		    (rsb.ep:client-configurator reader))))
    (rsb.ep:emit processor block?)))


;;; `reader' creation
;;

(defmethod make-reader ((scope scope)
			&key
			(transports (transport-options))
			(converters (default-converters)))
  (handler-bind
      ;; Translate different kinds of errors into
      ;; `reader-creation-failed' errors.
      ((error #'(lambda (condition)
		  (error 'reader-creation-failed
			 :scope      scope
			 :transports transports
			 :cause      condition))))
    (make-participant 'reader scope :in-pull transports converters)))

(define-participant-creation-uri-methods reader (scope puri:uri))

(define-participant-creation-restart-method reader (scope scope))
(define-participant-creation-restart-method reader (scope puri:uri))
