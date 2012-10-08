;;; informer.lisp --- An example program demonstration the listener class.
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
;; For managing the lifetime of listeners (e.g. for short-lived
;; listeners), the `with-listener' macro can used. It will take care
;; of disposing of the `listener' instance after it has been used,
;; also in case of non-local exist.
;;
;; To register a handler with limited lifetime, the `with-handler'
;; macro can be used.
;; mark-start::with-listener
(rsb:with-listener (listener "/example/listener2")
  (rsb::with-handler listener
      ((event)
       (format t "Received event: ~A~%" event))
    (sleep 20)))
;; mark-end::with-listener

;; This will create a `listener' instance that receives events which
;; are sent to the channel designated by the scope
;; "/example/listener". The listener will use all transports which are
;; enabled in the configuration with their respective configured
;; options.
;;
;; Just after creation, the listener will not act upon received
;; events. In order to process received events, handlers have to be
;; added to the listener. A handler is a function of one argument, the
;; event.
;;
;; The listener will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.
;; mark-start::variable
(defvar *listener* (rsb:make-listener "/example/listener"))

(push (lambda (event)
	(format t "Received event: ~A~%" event))
      (rsb.ep:handlers *listener*))
;; mark-end::variable

;; In order to be notified about event receiving errors, additional
;; error handlers have to be registered.
(push (lambda (condition)
	(format t "Error: ~A~%" condition))
      (hooks:hook-handlers (hooks:object-hook *listener* 'rsb:error-hook)))
;; mark-end::body
