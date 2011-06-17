;;; macros.lisp --- Unit tests for macros.
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
