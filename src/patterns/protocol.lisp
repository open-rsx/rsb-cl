;;; protocol.lisp --- Protocols used in the pattern module.
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

(in-package :rsb.patterns)


;;; Method protocol
;;

(defgeneric method-server (method)
  (:documentation
   "Return the server instance to which METHOD belongs."))

(defgeneric method-name (method)
  (:documentation
   "Return the name of METHOD."))


;;; Local and remote method invocation protocol
;;

(defgeneric call (server method request)
  (:documentation
   "Call METHOD of SERVER with argument REQUEST and return the result
of the method call. If a remote call fails for some reason, an error
of type `remote-call-failed' is signaled."))


;;; Server protocol
;;

(defgeneric server-methods (server)
  (:documentation
   "Return a list of methods provided by SERVER."))

(defgeneric server-method (server name
			   &key
			   error?)
  (:documentation
   "Return the method named NAME of SERVER. If no such method exists
and ERROR? is non-nil signal an error. Otherwise return nil."))

(defgeneric (setf server-method) (new-value server name)
  (:documentation
   "Store NEW-VALUE as the method named NAME of SERVER. NEW-VALUE can
be a method instance a thing like a function based on which a method
can be made. If NEW-VALUE is nil, the method stored for NAME is
removed from SERVER."))


;;; Remote server protocol
;;

(defgeneric make-remote-server (scope-or-uri
				&key
				transports)
  (:documentation
   "Make and return a `remote-server' instance that calls methods
provided by servers on the scope SCOPE-OR-URI. TRANSPORTS determines
the transport configuration that should be used to contact the
servers. See `rsb.transport::make-connectors' for details regarding
acceptable values of TRANSPORTS. If the server cannot be created, an
error of type `remote-server-creation-failed' is signaled."))


;;; Local server protocol
;;

(defgeneric make-local-server (scope-or-uri
			       &key
			       transports)
  (:documentation
   "Make and return a `local-server' instance that provides methods on
the scope SCOPE-OR-URI for other participants to call. TRANSPORTS
determines the transport configuration that should be used to make the
provided methods available to other participants. See
`rsb.transport::make-connectors' for details regarding acceptable
values of TRANSPORTS. If the server cannot be created, an error of
type `local-server-creation-failed' is signaled."))
