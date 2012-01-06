;;; scope.lisp --- Unit tests for scope class and related functions.
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

(deftestsuite scope-root (root)
  ()
  (:setup
   (clrhash rsb::*scopes*))
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

  (ensure-cases (invalid)
      '("/foo " "/!@#/+1" #("!@#" "foo") #("/") '("") '())
    (ensure-condition 'type-error
      (make-scope invalid))))

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
	  "Test interning of `scope' instances.")
  intern

  (ensure-cases (scope)
      `("/"
	"/foo"
	,(make-scope "/")
	,(make-scope "/foo")
	,(make-scope "/" :intern? t)
	,(make-scope "/foo" :intern? t))
    (let ((result-1 (make-scope scope :intern? t))
	  (result-2 (make-scope scope :intern? t))
	  (result-3 (make-scope scope)))
      (ensure (scope-interned? result-1))
      (ensure (scope-interned? result-2))
      ;; RESULT-3 can only be interned if SCOPE was of type `scope'
      ;; and was already interned.
      (ensure (eq (scope-interned? result-3)
		  (when (typep scope 'scope)
		    (scope-interned? scope))))

      (ensure-same result-1 result-2 :test #'eq)
      (ensure-same result-1 result-3 :test #'scope=))))

(addtest (scope-root
          :documentation
	  "Test method on `print-object' for `scope' class.")
  print

  (ensure-cases (scope intern?)
      '(("/"         nil) ("/"         t)
	("/foo"      nil) ("/foo"      t)
	("/foo/bar/" nil) ("/foo/bar/" t))
   (check-print (make-scope scope :intern? intern?))))
