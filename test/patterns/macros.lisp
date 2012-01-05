;;; macros.lisp --- Unit tests for macros of the patterns module.
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

(in-package :rsb.patterns.test)

(deftestsuite macros-root (patterns-root)
  ()
  (:documentation
   "Root test suite for tests of macros in the patterns module."))

(deftestsuite with-methods-root (macros-root)
  ()
  (:documentation
   "Test suite for the `with-methods' macro."))

(addtest (with-methods-root
          :documentation
	  "Smoke test for the `with-methods' macro.")
  smoke

  (with-local-server (server "inprocess:")
    (with-methods (server)
	(("mymethod"     (foo string) foo)
	 (:myothermethod (bar integer)
	  (declare (ignore bar))))
      (ensure (server-method server "mymethod"))
      (ensure (server-method server "MYOTHERMETHOD")))
    (ensure-null (server-methods server))))

(addtest (with-methods-root
          :documentation
	  "Test macroexpansion behavior of `with-methods' macro.")
  macroexpand

  (ensure-cases (method expected)
      '((("validname"    (foo string) foo)  t)
	((:validname     (foo string) foo)  t)
	((validname      (foo string) foo)  t)

	(("%invalidname" (foo string) foo) :error)
	(("invalid-name" (foo string) foo) :error)
	(("inv41id-n4m3" (foo string) foo) :error))

    (if (eq expected :error)
	(ensure-condition 'type-error
	  (macroexpand `(with-methods (server) (,method))))
	(macroexpand `(with-methods (server) (,method))))))
