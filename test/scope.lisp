;;; scope.lisp --- Unit tests for scope class and related functions.
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

(deftestsuite scope-root (root)
  ()
  (:documentation
   "Unit tests for the `scope' class."))

(addtest (scope-root
          :documentation
	  "Test constructing `scope' instances.")
  construction

  (ensure-same
   (scope-components (make-scope (make-scope "/foo")))
   '("foo")
   :test #'equalp)
  (ensure-same
   (scope-components (make-scope '("bar" "foo")))
   '("bar" "foo")
   :test #'equalp)
  (ensure-same
   (scope-components (make-scope (make-scope #("baz" "bar"))))
   '("baz" "bar")
   :test #'equalp)

  (ensure-condition 'type-error
    (make-scope "/!@#/+1"))
  (ensure-condition 'type-error
    (make-scope #("!@#" "foo")))
  (ensure-condition 'type-error
    (make-scope #("/")))
  (ensure-condition 'type-error
    (make-scope '(""))))

(addtest (scope-root
          :documentation
	  "Test constructing `scope' instances from strings.")
  from-string

  (ensure-cases (string components)
      '(("///Foo//BAR" ("Foo" "BAR"))
	("//foo/bar"   ("foo" "bar"))
	("foo/bar"     ("foo" "bar"))
	("//foo/bar"   ("foo" "bar"))
	("/foo/bar/"   ("foo" "bar"))
	("/foo/5/"     ("foo" "5"))
	("/"           ()))
    (ensure-same
     (scope-components (make-scope string))
     components
     :test #'equal)))

(addtest (scope-root
          :documentation
	  "Test relations between `scope' instances.")
  relations

  (ensure-cases (left right relation equality)
      '(("/foo"     "/foo"     (:sub :super) :=)
	("/foo/"    "/foo"     (:sub :super) :=)
	("/foo/bar" "/foo"     :sub          :/=)
	("/foo/bar" "/foo/baz" :none         :/=)
	("/"        "/foo"     :super        :/=)
	("/bar"     "/baz"     :none         :/=))

    (let ((left     (make-scope left))
	  (right    (make-scope right))
	  (relation (ensure-list relation)))
      ;; Test sub/super relation.
      (cond
	((member :sub relation)
	 (ensure (sub-scope?   left right))
	 (ensure (super-scope? right left)))
	((member :super relation)
	 (ensure (super-scope? left right))
	 (ensure (sub-scope?   right left)))
	((member :none relation)
	 (ensure (not (sub-scope?   left right)))
	 (ensure (not (super-scope? left right)))))

      ;; Test equality relation.
      (ecase equality
	(:=
	 (ensure (scope= left right)))
	(:/=
	 (ensure (not (scope= left right))))))))

(addtest (scope-root
          :documentation
	  "Test method on `print-object' for `scope' class.")
  print

  (check-print (make-scope "/"))
  (check-print (make-scope "/foo"))
  (check-print (make-scope "/foo/bar/")))
