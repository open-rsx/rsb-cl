;;; receiver.lisp --- An example program demonstrating the protocol buffer converter.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

;; In order to be able to use a protocol buffer data definition, a
;; data-holder and corresponding (de)serialization code has to be
;; generated. When a textual definition is available, this can be done
;; at runtime like this:
(let ((descriptor (pbf:load/text #P"SimpleImage.proto")))
  (pbb:emit descriptor :class)
  (pbb:emit descriptor :packed-size)
  (pbb:emit descriptor :serializer)
  (pbb:emit descriptor :deserializer))

;; Once the data-holder class and its (de)serialization methods have
;; been generated, any participant that is configured to use the
;; generic protocol buffer converter will be able to send and receive
;; events containing the protocol buffer message in question as a
;; payload:
(rsb:with-reader (reader "/example/protobuf")
  (let ((event (rsb:receive reader)))
   (format t "Received ~A with payload ~%" event)
   (describe event)))
