;;; types.lisp --- Types used in the cl-rsb system.
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

(deftype scope-component ()
  '(and string (satisfies scope-component?)))

(deftype scope-components ()
  '(or null
       (cons scope-component t)))

(defun scope-component? (string)
  "Non-nil when STRING is a valid scope component."
  (bind (((:flet valid-char? (char))
	  (or (<= (char-code #\a) (char-code char) (char-code #\z))
	      (<= (char-code #\A) (char-code char) (char-code #\Z))
	      (<= (char-code #\0) (char-code char) (char-code #\9)))))
    (and (not (emptyp string))
	 (every #'valid-char? string))))


;;; Event-related types
;;

(deftype sequence-number ()
  "Event sequence numbers are 32-bit unsigned integers."
  '(and fixnum (unsigned-byte 32)))


;;; Event-processing-related types
;;

(deftype error-policy ()
  "Objects of this type designate behaviors in case of errors."
  '(or null function))

(deftype implementation-feedback ()
  '(member :implemented :not-implemented))

(deftype direction ()
  '(member :in-push :in-pull :out))

(deftype wire-type ()
  "A certain type of data exchanged \"on the wire\" of a transport
mechanism."
  '(or symbol list))
