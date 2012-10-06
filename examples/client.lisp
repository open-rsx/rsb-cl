;;; client.lisp --- An example program demonstration the remote-server class.
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
;; Create a `remote-server' instance that calls methods of the remote
;; server at "/example/clientserver".
;; The remote server will use all transports which are enabled in the
;; global RSB configuration with their respective configured options.
(defvar *my-remote-server* (rsb.patterns:make-remote-server "/example/clientserver"))

;; Methods can be called without further preparation. Note that the
;; initial call of a method may take more time than subsequent methods
;; due to lazy initialization strategies.
(rsb.patterns:call *my-remote-server* "echo" "bla")

;; The remote server will remain connected to the bus until it is
;; garbage collected or explicitly detached using the `detach'
;; function.
(rsb:detach *my-remote-server*)

;; For managing the lifetime of `remote-server' instances (e.g. for
;; short-lived clients), the `with-remote-server' macro can used. It
;; will take care of disposing of the `remote-server' instance after
;; it has been used, also in case of non-local exist.
(rsb.patterns:with-remote-server (my-remote-server "/example/clientserver")

  (rsb.patterns:call my-remote-server "echo" "bla")

  ;; The default behavior of returning the reply payload can be
  ;; changed using the :return keyword parameter.
  (rsb.patterns:call my-remote-server "echo" "bla"
		     :return :event)

  ;; Non-blocking calls can be made using the :block? keyword
  ;; parameter. In that case, an object implementing the future
  ;; protocol is returned to represent the result of the computation.
  (let ((future (rsb.patterns:call my-remote-server "echo" "bla"
				   :block? nil)))
    (rsb.patterns:future-result future))

  ;; These behaviors can be combined:
  (let ((future (rsb.patterns:call my-remote-server "echo" "bla"
				   :block? nil
				   :return :event)))
    (rsb.patterns:future-result future))

  ;; Another way of calling methods makes use of the fact that
  ;; `remote-method' instances are funcallable:
  (map 'list (rsb.patterns:server-method my-remote-server "echo")
       '("a" "b" "c"))

  ;; This variant provides all the different behaviors of the `call'
  ;; variant:
  (funcall (rsb.patterns:server-method my-remote-server "echo")
	   "bla"
	   :return :event
	   :block? nil))
;; mark-end::body
