;;; protocol-buffer-xpath.lisp ---
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

(cl:in-package :rsb.transport.spread)

;; TODO(jmoringe): generic filtering-connector-mixin?

;; TODO(jmoringe): do this without redefinition
(defclass in-connector (connector
			restart-message-receiver-mixin
			broadcast-processor
			assembly-mixin
			expose-wire-schema-mixin
			rsb.transport.filter:event-filter->notification-filter-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
connector classes for Spread."))


;;; Envelope and meta-data filters
;;

(macrolet
    ((define-filter-translation (connector-class (filter-var filter-class)
				 &body doc-and-xpath-form)
       "Define a method on `make-notification-filter-for-filter' that
translate filter instances of FILTER-CLASS. FILTER-VAR can be used in
DOC-AND-XPATH-FORM to extract parameters from the filter."
       (check-type filter-var   symbol "a symbol")
       (check-type filter-class symbol "a symbol naming a class")

       (let+ (((&values xpath-form nil docstring)
	       (parse-body doc-and-xpath-form :documentation t)))
	 `(defmethod rsb.transport.filter:make-notification-filter-for-filter
	      ((connector   ,connector-class)
	       (,filter-var ,filter-class))
	    ,@(when docstring `(,docstring))
	    (when-let ((xpath (progn ,@xpath-form)))
	      (make-instance 'rsb.filter::protocol-buffer-xpath-filter
			     :xpath      xpath
			     :descriptor (load-time-value
					  (pb:find-descriptor
					   ".rsb.protocol.Notification") t)))))))

  (define-filter-translation in-connector
      (filter rsb.filter:origin-filter)
    "Translate `origin-filter' instances into XPath filters on the
Notification.meta_data.sender_id field."
    `(xpath:xpath
      ((:qfunc "pb" "equalp")
       ((:qfunc "pb" "value")
	(:path (:child :node)
	       (:child "meta_data")
	       (:attribute "sender_id")))
       ,(uuid:uuid-to-byte-array
	 (rsb.filter:filter-origin filter)))))

  ;; TODO(jmoringe): maybe this instead?
  ;; "node()/meta_data[equalp(value(@sender_id), UUID)]"
  ;; =>
  ;; `(xpath:xpath
  ;;   (:path (:child :node)
  ;;	   (:child "meta_data"
  ;;		   ((:qfunc "pb" "equalp")
  ;;		    ((:qfunc "pb" "value")
  ;;		     (:path (:attribute "sender_id")))
  ;;		    ,(uuid:uuid-to-byte-array *uuid*)))))

  (define-filter-translation in-connector
      (filter rsb.filter:method-filter)
    "Translate `method-filter' instances into XPath filters on the
Notification.method field."
    `(xpath:xpath
      (:path (:child :node
		     (= (:path (:attribute "method"))
			,(prin1-to-string
			  (sb-ext:string-to-octets
			   (rsb.filter:filter-method filter))))))))

  #+no-such-filter-yet
  (define-filter-translation in-connector
      (filter rsb.filter:meta-data-filter)
    "Translate `meta-data-filter' instances into XPath filters on the
Notification.meta_data.user_infos items."
    (let+ (((&accessors-r/o
	     (key       rsb.filter:filter-key)
	     (predicate rsb.filter:filter-predicate)) filter)
	   ((op &optional value) predicate))
      `(xpath:xpath
	(,op
	 (:string
	  (:path (:child :node)
		 (:child "meta_data")
		 (:child "user_infos"
			 (= (:path (:attribute "key")) ,key))
		 (:attribute "value")))
	 ,value))))

  #+no-such-filter-yet
  (define-filter-translation in-connecor
      (filter rsb.filter:transport-tag-filter)
    :TODO))


;;; Payload filters
;;

(macrolet
    ((define-payload-filter-translation (connector-class (filter-var filter-class)
					 &body doc-and-xpath-form)
       "Define a filter translation method that translates event
filters of class FILTER-CLASS into corresponding notification filters
suitable for the connector class CONNECTOR-CLASS."
       (check-type filter-var   symbol "a symbol")
       (check-type filter-class symbol "a symbol naming a class")

       (let+ (((&values xpath-form nil docstring)
	       (parse-body doc-and-xpath-form :documentation t)))
	 `(defmethod rsb.transport.filter:make-notification-filter-for-filter
	      ((connector   ,connector-class)
	       (,filter-var ,filter-class))
	    ,@(when docstring `(,docstring))
	    (let+ (((&values xpath descriptor) (progn ,@xpath-form))
		   (payload-filter (make-instance
				    'rsb.filter::protocol-buffer-xpath-filter
				    :xpath      xpath
				    :descriptor descriptor)))
	      ;; TODO(jmoringe): make a filter class instead?
	      #'(lambda (buffer)
		  ;;; TODO(jmoringe): match wire-schema first?
		  (rsb.filter:payload-matches?
		   payload-filter buffer
		   :offset (%payload-offset buffer))))))))

  (define-payload-filter-translation in-connector (filter rsb.filter:xpath-filter)
    (rsb.filter:filter-xpath filter))

  (define-payload-filter-translation in-connector (filter rsb.filter::protocol-buffer-xpath-filter)
    (values (rsb.filter:filter-xpath filter)
	    (rsb.filter::filter-descriptor filter))))

(defmethod rsb.transport.filter:make-notification-filter-for-filter
    ((connector in-connector)
     (filter    rsb.filter::protocol-buffer-xpath-filter))
  (let+ (((&accessors-r/o (xpath      rsb.filter:filter-xpath)
			  (descriptor rsb.filter::filter-descriptor)) filter)
	 (payload-filter (make-instance
			  'rsb.filter::protocol-buffer-xpath-filter
			  :xpath      xpath
			  :descriptor descriptor)))
    ;; TODO(jmoringe): make a filter class instead?
    ;; TODO(jmoringe): match wire-schema first?
    ;; maybe conjoin-filter (type-filter, payload-filter)?
    #'(lambda (buffer)
	(rsb.filter:payload-matches?
	 payload-filter buffer :offset (%payload-offset buffer)))))

;;; TODO(jmoringe): make a filter class; extensible descriptor lookup (for IDL download)
(defmethod rsb.transport.filter:make-notification-filter-for-filter
    ((connector in-connector)
     (filter    rsb.filter:xpath-filter))
  (let+ (((&accessors-r/o (xpath rsb.filter:filter-xpath)) filter)
	 (filters (make-hash-table :test #'eq))
	 ((&flet make-filter (wire-schema)
	    (log1 :info "Looking up descriptor for ~S" wire-schema)
	    (if-let ((descriptor (pb:find-descriptor wire-schema
						     :error? nil)))
	      (progn
		(log1 :info "Building filter for ~S" descriptor)
		(make-instance
		 'rsb.filter::protocol-buffer-xpath-filter
		 :xpath      xpath
		 :descriptor descriptor))
	      (log1 :warn "Could not find descriptor for ~S; not building filter"
		    wire-schema))))
	 ((&flet ensure-filter (wire-schema)
	    (or (gethash wire-schema filters)
		(setf (gethash wire-schema filters)
		      (make-filter wire-schema))))))
    #'(lambda (buffer)
	(when-let ((filter (ensure-filter (%extract-wire-schema buffer))))
	  (rsb.filter:payload-matches?
	   filter buffer :offset (%payload-offset buffer))))))


;;;
;;

(defun %extract-wire-schema (buffer)
  "TODO(jmoringe): document"
  (let ((notification (load-time-value
		       (pb:find-descriptor ".rsb.protocol.Notification") t))
	(wire-schema  (load-time-value
		       (pb:find-descriptor ".rsb.protocol.Notification.wire_schema") t)))
    (bytes->wire-schema
     (pb:extract buffer notification wire-schema))))

(defun %payload-offset (buffer)
  "TODO(jmoringe): document"
  (let ((notification (load-time-value
		       (pb:find-descriptor ".rsb.protocol.Notification") t))
	(data         (load-time-value
		       (pb:find-descriptor ".rsb.protocol.Notification.data") t)))
    (nth-value 2 (pb:offset buffer notification data))))


;;;
;;

(eval-when (:load-toplevel)
  (format t "~&XPath matching on packed protocol buffers is available!~%"))
