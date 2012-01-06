;;; payload-matching-mixin.lisp --- Mixin for payload-based filtering.
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

(in-package :rsb.filter)

(defclass payload-matching-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
discriminate event based on their payload."))

(defmethod matches? ((filter payload-matching-mixin) (event event))
  "Decide whether EVENT matches FILTER by calling `payload-matches?'
on the payload of EVENT."
  (case (payload-matches? filter (event-data event))
    ((nil)        nil)
    (:cannot-tell (call-next-method))
    (t            t)))

(defmethod payload-matches? ((filter payload-matching-mixin) (payload t)
			     &key &allow-other-keys)
  "The default behavior is not to decide based on the payload."
  :cannot-tell)
