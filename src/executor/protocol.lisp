;;; protocol.lisp --- Protocol provided by the executor module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.executor)


;;; Executor protocol
;;

(defgeneric submit (executor predicate thunk
		    &rest args)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric execute (executor)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric execute-one (executor predicate thunk
			 &rest args)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric collect-result (executor
			    &key
			    wait?)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric step1 (execcutor)
  (:documentation
   "TODO(jmoringe): document"))


;;; Threadpool executor protocol
;;

(defgeneric start (executor)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric shutdown (executor)
  (:documentation
   "TODO(jmoringe): document"))
