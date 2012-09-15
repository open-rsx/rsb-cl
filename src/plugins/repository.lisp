;;; repository.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.plugins)

(defvar *plugins* (make-hash-table :test #'equal)
  "TODO(jmoringe): document")

(defmethod providers ()
  )

(defmethod find-plugin ((name string)
			&key
			(if-does-not-exist #'error))
  (or (gethash name *plugins*)
      (etypecase if-does-not-exist
	(null
	 nil)
	(function
	 (restart-case
	     (funcall if-does-not-exist
		      (make-condition 'no-such-plugin
				      :plugin name))
	   (use-value (value)
	     value))))))
