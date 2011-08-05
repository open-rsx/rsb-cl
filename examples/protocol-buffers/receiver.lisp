;;; receiver.lisp --- An example program demonstrating the protocol buffer converter.
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
