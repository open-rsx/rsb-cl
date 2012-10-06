;;; server.lisp --- An example program demonstration the local-server class.
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

;; mark-start::body
;; Create a `local-server' instance that offers its methods under the
;; scope "/example/clientserver".
;; The local server will use all transports which are enabled in the
;; global RSB configuration with their respective configured options.
(defvar *my-local-server* (rsb.patterns:make-local-server "/example/clientserver"))

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
;; mark-end::body
