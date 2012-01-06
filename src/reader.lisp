;;; reader.lisp --- Pull-based receiving participant class.
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
