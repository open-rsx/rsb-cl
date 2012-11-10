;;; reloading.lisp --- Support for image dumping and reloading.
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

(cl:in-package :rsb)

(defun enable-restart-threadpool ()
  "Make sure that the rsbag threadpool is shutdown when saving a core
and restarted when loading a core."
  #+sbcl (progn
	   (push 'stop-threadpool  sb-ext:*save-hooks*)
	   (push 'start-threadpool sb-ext:*init-hooks*))
  #-sbcl (error "Restarting the rsbag threadpool is not supported in this Lisp."))
