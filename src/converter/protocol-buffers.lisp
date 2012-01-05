;;; protocol-buffers.lisp --- Converter for protocol buffer wire schemas.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(in-package :rsb.converter)

(defmethod wire->domain? ((converter   (eql :protocol-buffer))
			  (wire-data   simple-array)
			  (wire-schema symbol))
  (let* ((descriptor (pb:find-descriptor wire-schema
					 :error? nil))
	 (class      (when descriptor
		       (pb:descriptor-class descriptor))))
    (when class
      (values converter (class-name class)))))

(defmethod domain->wire? ((converter     (eql :protocol-buffer))
			  (domain-object standard-object))
  (when (pb:message-descriptor domain-object)
    (values
     converter 'octet-vector (class-name (class-of domain-object)))))

(defmethod wire->domain ((converter   (eql :protocol-buffer))
			 (wire-data   simple-array)
			 (wire-schema symbol))
  (check-type wire-data octet-vector)

  (let* ((descriptor (pb:find-descriptor (string wire-schema)))
	 (class      (pb:descriptor-class descriptor)))
    (nth-value 0 (pb:unpack wire-data class))))

(defmethod domain->wire ((converter     (eql :protocol-buffer))
			 (domain-object standard-object))
  (let* ((descriptor  (pb:message-descriptor domain-object))
	 (wire-schema (intern (pb:descriptor-qualified-name descriptor)
			      :keyword)))
   (values (pb:pack* domain-object) wire-schema)))
