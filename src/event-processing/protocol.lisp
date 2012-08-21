;;; protocol.lisp --- Protocols provided by the event-processing module.
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

(cl:in-package :rsb.event-processing)


;;; Push source protocol
;;
;; This protocol is provided by event sources that emit events without
;; requiring an external trigger, for example a thread polling a
;; network connection.

(defgeneric handlers (source)
  (:documentation
   "Return the handlers associated to SOURCE."))

(defgeneric (setf handlers) (new-value source)
  (:documentation
   "Set the list of handlers associated to SOURCE to NEW-VALUE."))


;;; Pull source protocol
;;
;; This protocol is provided by event sources that emit events only
;; after being triggered externally. This external triggering is
;; achieve by calling `emit' with such a source.

(defgeneric emit (source block?)
  (:documentation
   "Block until an event is available from SOURCE."))


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


;;; Dispatching processor protocol
;;

(defgeneric dispatch (processor event)
  (:documentation
   "Dispatch EVENT in the manner implemented by PROCESSOR. This may
mean to call some handlers, for example."))


;;; Notification protocol
;;

(defgeneric notify (recipient subject action)
  (:documentation
   "When ACTION is either :filter-added or :filter-removed, methods
should return one of the symbols :not-implemented or :implemented to
indicate whether the combination of the filter SUBJECT and ACTION
could be implemented by RECIPIENT."))


;;; Default behavior
;;

(defmethod notify ((recipient t) (subject t) (action t))
  "The default behavior is to do nothing."
  (values))

(defmethod notify ((recipient t)
		   (subject   t)
		   (action    (eql :filter-added)))
  "The default behavior for filter actions is to do nothing and state
the fact."
  :not-implemented)

(defmethod notify ((recipient t)
		   (subject   t)
		   (action    (eql :filter-removed)))
  "The default behavior for filter actions is to do nothing and state
the fact."
  :not-implemented)


;;; Error policy protocol
;;

(defgeneric apply-error-policy (processor condition)
  (:documentation
   "Apply the error handling policy of PROCESSOR to CONDITION."))


;;; Tagging processor protocol
;;

(defgeneric processor-tags (processor)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric (setf processor-tags) (new-value processor)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric processor-tag? (processor)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric (setf processor-tag?) (new-value processor)
  (:documentation
   "TODO(jmoringe): document"))


;;; Configurator protocol
;;

(defgeneric make-processor (configurator
			    &rest args
			    &key &allow-other-keys)
  (:documentation
   "Make and return a suitable processor instance for
CONFIGURATOR. Methods of this generic function will usually call
`collect-processor-mixins' and `ensure-processor-class' obtain the
desired class of the processor being made. ARGS are passed to
`make-instance' after the class has been determined."))

(defgeneric collect-processor-mixins (configurator)
  (:method-combination append)
  (:documentation
   "Return a list of names of mixin classes which should be combined
to make the processor class for CONFIGURATOR."))


;;; Processor classes
;;

(dynamic-classes:define-dynamic-class-family processor
    "This class family consists of dynamically constructed processor
classes. This facility can be used to construct appropriate processor
classes based on configuration information supplied at runtime.")
