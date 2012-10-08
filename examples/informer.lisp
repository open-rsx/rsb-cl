;;; informer.lisp --- An example program demonstration the informer class.
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
;; For managing the lifetime of informers (e.g. for short-lived
;; informers), the `with-informer' macro can used. It will take care
;; of disposing of the `informer' instance after it has been used,
;; also in case of non-local exist.
;; mark-start::with-informer
(rsb:with-informer (informer "/example/informer" 'string)
  (rsb:send informer "example payload"))
;; mark-end::with-informer

;; The following code will create an `informer' instance that
;; publishes events to the channel designated by the scope
;; "/example/informer" and is restricted to event payloads of type
;; string. The informer will use all transports which are enabled in
;; the configuration with their respective configured options.
;;
;; This will publish the string "data" to the channel in which the
;; informer participates.
;;
;; The informer will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.
;; mark-start::variable
(defvar *informer* (rsb:make-informer "/example/informer" 'string))

(rsb:send *informer* "example payload")
;; mark-end::variable
;; mark-end::body
