;;; reloading.lisp --- Code to be executed at startup/shutdown.
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

(defun enable-id-random-state-reseed ()
  "Reseed the `random-state' stored in `*id-random-state*'.

This function can be called to ensure that `*id-random-state*' is
reseeded at image startup thereby ensuring different pseudo random ids
and such for subsequent program runs."
  #+sbcl
  (push (lambda () (setf *id-random-state* (make-random-state t)))
	sb-ext:*init-hooks*))
