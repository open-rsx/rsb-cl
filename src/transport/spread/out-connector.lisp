;;; out-connector.lisp ---
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

(in-package :rsb.transport.spread)

(defmethod find-transport-class ((spec (eql :spread-out)))
  (find-class 'out-connector))

(defclass out-connector (connector) ;; is an "event-sink"; there is no class or interface for this
  ((direction :initform :out))
  (:documentation
   "DOC"))

;; (defmethod initialize-instance :after ((instance out-connector)
;;				       &key)
;;   (with-slots (connection) instance
;;     (hooks:add-to-hook (hooks:object-hook connection 'spread:join-hook)
;;		       (curry #'handle-join instance))
;;     (hooks:add-to-hook (hooks:object-hook connection 'spread:leave-hook)
;;		       (curry #'handle-leave instance))))
;;
;; (defmethod handle-join ((connector out-connector)
;;			(group     string)
;;			(members   list))
;;   "DOC"
;;   (when (length= 2 members)
;;     (log1 :info "~A: first member in our group ~S" connector group))
;;   )
;;
;; (defmethod handle-leave ((connector out-connector)
;;			 (group     string)
;;			 (members   list))
;;   "DOC"
;;   (when (length= 1 members)
;;     (log1 :info "~A: no more members in our group ~S" connector group))
;;   )

(defmethod rsb.ep:handle ((connector out-connector) (event event))
  (bind ((group-names   (scope->groups (event-scope event)))
	 (notifications (event->notifications event))) ;; TODO only if group is populated?
    ;; Due to large events being fragmented into multiple notifications,
    ;; we obtain a list of notifications here.
    (iter (for notification in notifications)
	  (send-message (slot-value connector 'connection)
			group-names
			(pb::pack1 notification)))))
