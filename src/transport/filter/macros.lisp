;;; macros.lisp --- Convenience macros for transport-level filter construction.
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

(cl:in-package :rsb.transport.filter)

(defmacro define-filter-translation (connector-class (filter-var filter-class)
				     &body doc-and-body)
  "Define a method on `make-notification-filter-for-filter' that
translates filter instances of FILTER-CLASS. DOC-AND-BODY should
return a filter object that implements behavior equivalent to
FILTER-VAR on notification processed by CONNECTOR-CLASS. FILTER-VAR
can be used in DOC-AND-BODY to extract parameters from the filter."
  (check-type connector-class (or symbol list) "a specializer")
  (check-type filter-var      symbol           "a symbol")
  (check-type filter-class    (or symbol list) "a specializer")

  (let+ (((&values body nil docstring)
	  (parse-body doc-and-xpath-form :documentation t)))
    `(defmethod make-notification-filter-for-filter
	 ((connector   ,connector-class)
	  (,filter-var ,filter-class))
       ,@(when docstring `(,docstring))
       (when-let ((xpath (progn ,@xpath-form)))
	 (make-instance 'rsb.filter::protocol-buffer-xpath-filter
			:xpath      xpath
			:descriptor (load-time-value
				     (pb:find-descriptor
				      ".rsb.protocol.Notification") t))))))
