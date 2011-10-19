;;; macros.lisp --- Unit tests for macros of the patterns module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

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