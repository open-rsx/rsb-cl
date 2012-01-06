;;; informer.lisp --- An example program demonstration the reader class.
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
