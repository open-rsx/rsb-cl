;;; executor.lisp --- Exectuor implementations.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package #:rsb.executor)

(defclass standard-executor ()
  ((tasks :reader   %tasks
	  :initform (lparallel.queue:make-queue)
	  :documentation
	  "Stores a sorted list of submitted tasks, not yet being
executed."))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod submit ((executor  standard-executor)
		   (predicate (eql t))
		   (thunk     function)
		   &rest args)
  (lparallel.queue:push-queue (list* predicate thunk args) (%tasks executor)))

(defmethod execute ((executor standard-executor))
  (apply #'execute-one executor (lparallel.queue:pop-queue (%tasks executor))))

(defmethod execute-one ((executor  standard-executor)
			(predicate (eql t))
			(thunk     function)
			&rest args)
  (apply thunk args))

(defmethod step1 ((executor standard-executor))
  (iter (for task in (execute executor))
	(while task)
	(apply #'submit executor task)))


;;; Threadpool executor
;;

(defconstant +request-shutdown+
  (if (boundp '+request-shutdown+) (symbol-value '+request-shutdown+)
      (gensym)))

(defclass threadpool-executor (standard-executor)
  ((pending   :type     alexandria:non-negative-integer
	      :accessor %pending
	      :initform 0
	      :documentation
	      "Stores the number of ")
   (results   :reader   %results
	      :initform (lparallel:make-channel)
	      :documentation
	      "")
   (scheduler :reader   %scheduler
	      :initform (lparallel:make-channel)
	      :documentation
	      "Stores a channel which is used to eventually join
scheduler task."))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod start ((executor threadpool-executor))
  "TODO(jmoringe): document"
  (lparallel:submit-task
   (%scheduler executor)
   #'(lambda ()
       (restart-case (iter (step1 executor))
	 (abort ())))))

(defmethod shutdown ((executor threadpool-executor))
  ;; Submit a task which will cause the scheduler to eventually
  ;; terminate after finishing all pending work.
  (submit executor +request-shutdown+ #'(lambda ()))

  ;; Wait for the scheduler to finish and join it.
  (lparallel:receive-result (%scheduler executor)))

(defmethod submit :after ((executor  threadpool-executor)
			  (predicate t)
			  (thunk     t)
			  &rest args)
  (declare (ignore args))

  (incf (%pending executor)))

(defmethod step1 ((executor threadpool-executor))
  (execute executor)
  (iter (while (collect-result executor))))

(defmethod collect-result ((executor threadpool-executor)
			   &key wait?)
  (let+ (((&values result result?)
	  (if wait?
	      (values (lparallel:receive-result (%results executor)) t)
	      (lparallel:try-receive-result (%results executor)))))
    (when result?
     (decf (%pending executor))
     (iter (for predicate-thunk-and-args in result)
	   (apply #'submit executor predicate-thunk-and-args))
     t)))

(defmethod execute-one ((executor  threadpool-executor)
			(predicate symbol) #+later (eql +request-shutdown+)
			(thunk     function)
			&rest args)
  "Execute shutdown request by executing all pending requests,
collecting their results and then aborting the scheduler(?) task."
  (declare (ignore args))

  (decf (%pending executor))
  (iter (until (and (lparallel.queue:queue-empty-p (%tasks executor))
		    (zerop (%pending executor))))
	(if (lparallel.queue:queue-empty-p (%tasks executor))
	    (collect-result executor :wait? t)
	    (step1 executor)))

  (abort))

(defmethod execute-one ((executor  threadpool-executor)
			(predicate t)
			(thunk     function)
			&rest args)
  "TODO(jmoringe): document"
  (apply #'lparallel:submit-task (%results executor) thunk args))

(defmethod print-object ((object threadpool-executor) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((num-tasks (lparallel.queue:queue-count (%tasks object))))
      (format stream "(~D|~D)"
	      num-tasks (- (%pending object) num-tasks)))))


;;;
;;

(defclass socket-mixin ()
  ((sockets :type     hash-table
	    :reader   %sockets
	    :initform (make-hash-table :test #'eq)
	    :documentation
	    ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod submit ((executor  socket-mixin)
		   (predicate usocket:usocket)
		   (thunk     function)
		   &key args)
  "TODO(jmoringe): document"
  (let+ (((&accessors-r/o (sockets %sockets)) executor))
    (setf (gethash predicate sockets) (list* thunk args))

    (submit executor t
	    (lambda ()
	      (iter (for ready in (usocket:wait-for-input
				   (hash-table-keys sockets)))
		    (collect (list* t (gethash ready sockets))))))))

(defclass threadpool-executor-with-sockets (socket-mixin
					    threadpool-executor)
  ()
  (:documentation
   "TODO(jmoringe): document"))
