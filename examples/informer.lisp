;;; informer.lisp --- An example program demonstration the informer class.
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

;; This will create an `informer' instance that publishes events to
;; the channel designated by the scope "/example/informer" and is
;; restricted to event payloads of type string. The informer will use
;; all transports which are enabled in the configuration with their
;; respective configured options.
(defvar *my-informer* (rsb:make-informer "/example/informer" 'string))

;; This will publish the string "my-data" to the channel in which the
;; informer participates.
(rsb:send *my-informer* "my-data")

;; The informer will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.

;; For managing the lifetime of informers (e.g. for short-lived
;; informers), the `with-informer' macro can used. It will take care
;; of disposing of the `informer' instance after it has been used,
;; also in case of non-local exist.
(rsb:with-informer (my-informer "/example/informer" 'string)
  (rsb:send my-informer "my-data"))
