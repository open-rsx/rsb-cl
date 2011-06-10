;;; informer.lisp --- An example program demonstration the reader class.
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

;; This will create a `reader' instance that receives events which are
;; sent to the channel designated by the scope "/example/reader". The
;; reader will use all transports which are enabled in the
;; configuration with their respective configured options.
(defvar *my-reader* (rsb:make-reader "/example/reader"))

;; Note: this form will block until an event is received.
(let ((event (rsb:receive *my-reader* :block? t))) ;; block? defaults to t
  (format t "Received event: ~A~%" event)
  event) ;; return the event

;; It is also possible to use `rsb:receive' in a non-blocking mode. In
;; that case, an `event' or nil is returned.
(let ((event (rsb:receive *my-reader* :block? nil)))
  (format t "~:[Did not receive an event~;Received event: ~:*~A~]~%"
	  event)
  event) ;; return the event, or nil

;; The reader will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.

;; For managing the lifetime of readers (e.g. for short-lived
;; readers), the `with-reader' macro can used. It will take care of
;; disposing of the `reader' instance after it has been used, also in
;; case of non-local exist.
;;
;; Note: this form will block until an event is received.
(rsb:with-reader (my-reader "/example/reader")
  (let ((event (rsb:receive my-reader :block? t)))
    (format t "Received event: ~A~%" event)
    event)) ;; return the event
