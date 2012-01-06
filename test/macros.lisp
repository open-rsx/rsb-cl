;;; macros.lisp --- Unit tests for macros.
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

(in-package :rsb.test)

(deftestsuite macros-root (root
			   participant-suite)
  ()
  (:documentation
   "Unit tests for macros provided by the cl-rsb system."))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-listener' macro.")
  with-listener-smoke

  (with-listener (listener "/listener")
    (ensure (typep listener 'listener))
    (check-participant listener "/listener")))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-reader' macro.")
  with-reader-smoke

  (with-reader (reader "/reader")
    (ensure (typep reader 'reader))
    (check-participant reader "/reader")))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-informer' macro.")
  with-informer-smoke

  (with-informer (informer "/informer" t)
    (ensure (typep informer 'informer))
    (check-participant informer "/informer")))
