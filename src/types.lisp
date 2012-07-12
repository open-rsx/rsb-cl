;;; types.lisp --- Types used in the cl-rsb system.
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

(cl:in-package :rsb)

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
  (let+ (((&flet valid-char? (char)
	    (or (<= (char-code #\a) (char-code char) (char-code #\z))
		(<= (char-code #\A) (char-code char) (char-code #\Z))
		(<= (char-code #\0) (char-code char) (char-code #\9))))))
    (and (not (emptyp string))
	 (every #'valid-char? string))))


;;; Event-related types
;;

(deftype sequence-number ()
  "Event sequence numbers are 32-bit unsigned integers."
  '(unsigned-byte 32))

(deftype event-id ()
  "A pair of an origin id and sequence that uniquely identifies and
event."
  '(cons uuid:uuid sequence-number))


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
