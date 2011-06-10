;;; informer.lisp --- An example program demonstration the listener class.
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

;; This will create a `listener' instance that receives events which
;; are sent to the channel designated by the scope
;; "/example/listener". The listener will use all transports which are
;; enabled in the configuration with their respective configured
;; options.
(defvar *my-listener* (rsb:make-listener "/example/listener"))

;; Just after creation, the listener will not act upon received
;; events. In order to process received events, handlers have to be
;; added to the listener. A handler is a function of one argument, the
;; event.
(push (lambda (event)
	(format t "Received event: ~A~%" event))
      (rsb.ep:handlers *my-listener*))

;; In order to be notified about event receiving errors, additional
;; error handlers have to be registered.
(push (lambda (condition)
	(format t "Error: ~A~%" condition))
      (hooks:hook-handlers (hooks:object-hook *my-listener* 'rsb:error-hook)))

;; The listener will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.

;; For managing the lifetime of listeners (e.g. for short-lived
;; listeners), the `with-listener' macro can used. It will take care
;; of disposing of the `listener' instance after it has been used,
;; also in case of non-local exist.
;;
;; To register a handler with limited lifetime, the `with-handler'
;; macro can be used.
(rsb:with-listener (my-listener "/example/listener2")
  (rsb::with-handler my-listener
      ((event)
       (format t "Received event: ~A~%" event))
    (sleep 20)))
