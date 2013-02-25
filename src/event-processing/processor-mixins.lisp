;;; processor-mixins.lisp --- Mixin classes for processor classes.
;;
;; Copyright (C) 2013 Jan Moringen
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

;;; `transform-mixin' class

(defclass transform-mixin ()
  ((transform :initarg  :transform
	      :reader   processor-transform
	      :documentation
	      "Stores a transform (in the sense of being usable with
`transform!') that should be applied to all handled data."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-mixin :transform))
  (:documentation
   "This mixin class adds to processor classes the ability to apply a
transform (in the sense of being usable with `transform!') to all
handled data."))

(defmethod handle :around ((sink transform-mixin) (data t))
  (call-next-method sink (transform! (processor-transform sink) data)))
