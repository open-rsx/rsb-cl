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

(cl:in-package :rsb.filter)

(defmethod find-filter-class ((spec (eql :protocol-buffer-xpath)))
  (find-class 'protocol-buffer-xpath-filter))

(defclass protocol-buffer-xpath-filter (xpath-filter)
  ((descriptor :initarg  :descriptor
	       :type     pb:message-desc
	       :reader   filter-descriptor
	       :initform (pb:find-descriptor ".rsb.protocol.Notification") ;;; TODO(jmoringe):  temp
	       :documentation
	       ""))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod compile-xpath ((filter protocol-buffer-xpath-filter)
			  (xpath  string))
  "Compile the XPath."
  (protobuf-and-xpath:compile-xpath xpath))

;;; TODO(jmoringe): hack
(defmethod matches? ((filter protocol-buffer-xpath-filter)
		     (thing  simple-array))
  (payload-matches? filter thing))

(defmethod payload-matches? ((filter  protocol-buffer-xpath-filter)
			     (payload standard-object)
			     &key &allow-other-keys)
  "Match the data of EVENT against the XPath of FILTER."
  (let+ (((&accessors-r/o
	   (compiled-xpath filter-compiled-xpath)) filter)
	 (document (make-instance 'document :root payload))) ;;; TODO(jmoringe):  temp
    (xpath-result->filter-result
     (xpath:evaluate-compiled compiled-xpath document t))))

;;; TODO(jmoringe): maybe this should be a different kind of xpath filter?
(defmethod payload-matches? ((filter  protocol-buffer-xpath-filter)
			     (payload simple-array)
			     &key
			     (offset 0)
			     &allow-other-keys)
  "Match the data of EVENT against the XPath of FILTER."
  (check-type payload octet-vector "an octet-vector")

  (let+ (((&accessors-r/o (compiled-xpath filter-compiled-xpath)
			  (descriptor     filter-descriptor)) filter))
    (xpath-result->filter-result (protobuf-and-xpath:evaluate-xpath
				  compiled-xpath payload descriptor
				  :start offset))))
