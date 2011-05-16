;;; types.lisp ---
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

(in-package :rsb)

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))


;;; Scope-related types
;;

(deftype scope-anchor ()
  '(member :relative :absolute))

(deftype scope-component ()
  '(and string (satisfies scope-component?)))

(deftype scope-components ()
;;'(cons scope-anchor
         '(or null
	     (cons scope-component t)))

(defun scope-component? (string)
  "DOC"
  (bind (((:flet valid-char? (char))
	  (or (<= (char-code #\a) (char-code char) (char-code #\z))
	      (<= (char-code #\A) (char-code char) (char-code #\Z))
	      (<= (char-code #\0) (char-code char) (char-code #\9)))))
    (and (not (emptyp string))
	 (every #'valid-char? string))))


;;;
;;


(deftype implementation-feedback ()
  '(member :implemented :not-implemented))

(deftype direction ()
  '(member :in-push :in-pull :out))

(deftype wire-schema-designator ()
  "One of the forms:
\(MECHANISM . MECHANISM-SPECIFIC-DESIGNATOR)"
  `(or (cons keyword symbol)
       ))
