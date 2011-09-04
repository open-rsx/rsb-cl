;;; server.lisp --- An example program demonstration the local-server class.
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

;; Note: depending on your RSB configuration, this example may require
;; a running Spread daemon for successful execution.

;; Create a `local-server' instance that offers its methods under the
;; scope "/example/clientserver".
;; The local server will use all transports which are enabled in the
;; global RSB configuration with their respective configured options.
(defvar *my-local-server* (rsb.patterns:make-local-server "inprocess:/example/clientserver"))

;; The new server instance initially does not have any methods. There
;; are several ways to add methods.
(setf (rsb.patterns:server-method *my-local-server* "echo")
      #'(lambda (arg)
	  arg))

;; The local server and its methods will remain connected to the bus
;; until they are garbage collected or explicitly detached using the
;; `detach' function.
(rsb:detach *my-local-server*)

;; For managing the lifetime of `local-server' instances (e.g. for
;; short-lived clients), the `with-local-server' macro can used. It
;; will take care of disposing of the `remote-server' instance after
;; it has been used, also in case of non-local exist.
(rsb.patterns:with-local-server (my-server "/example/clientserver")

  (setf (rsb.patterns:server-method my-server "echo")
	#'(lambda (arg)
	    arg))

  ;; Methods can be managed similarly. After the `with-methods' form,
  ;; the methods are removed.
  (rsb.patterns:with-methods (my-server)
      (("echo2" (arg string)
          arg))))
