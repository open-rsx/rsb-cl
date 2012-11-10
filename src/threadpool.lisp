;;; threadpool.lisp --- A threadpool for async rsb operations.
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package #:rsb)

(defvar *threadpool* nil
  "When non-nil, holds the threadpool used by the rsb system.")

(defun start-threadpool (&key (threads 4))
  "Create and initialize a threadpool for use by the rsb system."
  (setf *threadpool* (lparallel:make-kernel threads :name "rsb")))

(defun stop-threadpool ()
  "Stop all threads of the threadpool used by the rsb system."
  (let ((lparallel:*kernel* *threadpool*))
    (lparallel:end-kernel :wait t))
  (setf *threadpool* nil))

(defmacro with-threadpool (&body body)
  "Execute BODY such that created tasks use the rsb threadpool and
errors are transferred."
  `(let ((lparallel:*kernel* *threadpool*))
     (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
       ,@body)))

;; Start the threadpool when loading or executing this. It may have to
;; be stopped and restarted when saving an image. See reloading.lisp.
(start-threadpool)
