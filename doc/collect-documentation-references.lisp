;;; collect-documentation-references.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>
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

#.(progn
    (ql:quickload '(:alexandria :iterate :let-plus :cxml-stp :xpath :drakma :cl-fad
		    ;; dependencies of cl-rsb:
		    :cl-hooks :more-conditions :nibbles))
    (values))

(cl:defpackage :rsb.doc.check-references
  (:use
   :cl
   :alexandria
   :iterate
   :let-plus)

  (:export
   :*documentation-references*)

  (:export
   :check-references)

  (:documentation
   "TODO(jmoringe): document"))

(cl:in-package :rsb.doc.check-references)

(declaim (special *documentation-references*))

(defvar *documentation-references* nil
  "TODO(jmoringe): document")

(defun collect-references (system &optional load)
  "TODO(jmoringe): document"
  (mapc #'load load)

  (let ((symbol (read-from-string "rsb::documentation-ref/rsb-manual")))
   (eval
    `(define-compiler-macro ,symbol (&whole whole &rest parts)
       "TODO(jmoringe): document"
       (let ((spec (apply ',symbol parts)))
	 (format t "~@<~A: ~<~A, ~{~A~^ » ~}~_<~A>~:>~@:>~%"
		 *compile-file-pathname* spec)
	 (pushnew spec *documentation-references*
		  :test #'equal))
       whole)))

  (let ((old-value                  asdf:*output-translations-parameter*)
	(*documentation-references* nil))
    (unwind-protect
	 (let ((*compile-print*   nil)
	       (*compile-verbose* nil))
	   (asdf:initialize-output-translations
	    `(:output-translations
	      (,(namestring
		 (asdf:component-pathname (asdf:find-system system)))
	       "/tmp/check-documentation-references-fasls/")
	      :inherit-configuration))
	   (handler-bind
	       ((error     (lambda (condition)
			     (when (find-restart 'continue)
			       (invoke-restart 'continue))))
		(condition (lambda (condition)
			     (when (find-restart 'muffle-warning)
			       (invoke-restart 'muffle-warning)))))
	     (asdf:load-system system))
	   *documentation-references*)
      (setf asdf:*output-translations-parameter* old-value)
      (ignore-errors
       (cl-fad:delete-directory-and-files "/tmp/check-documentation-references-fasls/")))))

(defun check-references (&optional (references *documentation-references*))
  "TODO(jmoringe): document"
  (let ((cxml:*catalog* (handler-bind ((warning #'muffle-warning))
			  (cxml:make-catalog))))
    (iter (for (document section url) in references)

	  (let+ (((&accessors-r/o (scheme   puri:uri-scheme)
				  (path     puri:uri-path)
				  (fragment puri:uri-fragment)) (puri:uri url))
		 ((&values content code)
		  (ecase scheme
		    (:http (drakma:http-request url :want-stream t))
		    (:file (if (probe-file path)
			       (values (open path :element-type '(unsigned-byte 8)) 200)
			       (values nil 404))))))
	    (unless (= code 200)
	      (warn "~@<Broken link: ~A, ~{~A~^ » ~} ~A => ~D~@:>"
		    document section url code)
	      (next-iteration))

	    (when fragment
	      (let+ (((&flet resolver (pubid sysid)
			(declare (ignore pubid))
			(when (eq (puri:uri-scheme sysid) :http)
			  (drakma:http-request sysid :want-stream t))))
		     (dom (cxml:parse content (stp:make-builder)
				      ;; :entity-resolver #'resolver
				      )))
		(when (xpath:node-set-empty-p
		       (xpath:evaluate (format nil "//node()[@id='~A']" fragment) dom))
		  (warn "~@<Broken link anchor: ~A, ~{~A~^ » ~} ~A: attribute id='~A' not in document~@:>"
			document section url fragment)
		  (next-iteration))))))))

(defun collect+check-references (system &optional (load (list (asdf:system-relative-pathname system "src/package.lisp")
							      (asdf:system-relative-pathname system "src/error-handling.lisp"))))
  "TODO(jmoringe): document"
  (check-references (collect-references system load)))

(sb-ext:save-lisp-and-die "check-documentation-references"
			  :executable t
			  :toplevel   #'(lambda ()
					  (handler-case
					      (collect+check-references :cl-rsb)
					    (error (condition)
					      (format *error-output* "~A~%" condition)))))
