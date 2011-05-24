;;; out-connector.lisp ---
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

(in-package :rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-out)))
  (find-class 'out-connector))

(defclass out-connector (connector)
  ((direction :initform :out))
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod handle ((connector out-connector) (event event))
  (iter (for super in (super-scopes (event-scope event)
				    :include-self? t))
	(handle (by-scope super) event)))

;; TODO we could implement an unfiltered semantic here:
;; (t-sinks (gethash t *by-scope*))
;; (map nil (rcurry #'handle event) t-sinks)
