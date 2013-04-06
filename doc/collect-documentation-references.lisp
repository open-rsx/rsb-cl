;;; collect-documentation-references.lisp ---
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(cl:defpackage #:rsb.doc.check-references
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus)

  (:export
   #:*documentation-references*)

  (:export
   #:check-references)

  (:documentation
   "TODO(jmoringe): document"))

(cl:in-package #:rsb.doc.check-references)

(defvar *fasl-directory* "/tmp/check-documentation-references-fasls/"
  "TODO(jmoringe): document")

(declaim (special *documentation-references*))

(defvar *documentation-references* nil
  "TODO(jmoringe): document")

(defun install-collect-macro (symbol)
  (let ((symbol (etypecase symbol
		  (symbol symbol)
		  (string (read-from-string symbol)))))
    (eval
     `(define-compiler-macro ,symbol (&whole whole &rest parts)
	"TODO(jmoringe): document"
	(let ((spec (apply ',symbol parts)))
	  (format t "~@<~A: ~<~A, ~{~A~^ » ~}~_<~A>~:>~@:>~%"
		  *compile-file-pathname* spec)
	  (pushnew spec *documentation-references*
		   :test #'equal))
	whole))))

(defmacro with-system-fasl-directory
    ((system &optional (directory '*fasl-directory*))
     &body body)
  "TODO(jmoringe): document"
  (once-only (system directory)
    (with-gensyms (old-value)
      `(let ((,old-value  asdf:*output-translations-parameter*))
	 (unwind-protect
	      (progn
		(asdf:initialize-output-translations
		 `(:output-translations
		   (,(namestring
		      (asdf:component-pathname (asdf:find-system ,system)))
		    ,,directory)
		   :inherit-configuration))
		,@body)
	   (setf asdf:*output-translations-parameter* ,old-value)
	   (ignore-errors
	    (cl-fad:delete-directory-and-files ,directory)))))))

(defmacro with-silent-compilation (&body body)
  `(let ((*compile-print*   nil)
	 (*compile-verbose* nil)
	 (*load-print*      nil)
	 (*load-verbose*    nil))
     (handler-bind
	 ((condition (lambda (condition)
		       (declare (ignore condition))
		       (when (find-restart 'muffle-warning)
			 (invoke-restart 'muffle-warning)))))
       ,@body)))

(defun compile-system-silently (system)
  (with-system-fasl-directory (system)
    (with-silent-compilation
      (asdf:load-system system))))

(defun collect-references (system &optional load)
  "TODO(jmoringe): document"
  (with-silent-compilation (mapc #'load load))

  (install-collect-macro "rsb:documentation-ref/rsb-bug")
  (install-collect-macro "rsb:documentation-ref/rsb-glossary")
  (install-collect-macro "rsb:documentation-ref/rsb-manual")

  (let ((*documentation-references* '()))
    (compile-system-silently system)
    *documentation-references*))

(defun+ check-reference ((&whole spec document section url))
  (let ((cxml:*catalog* (handler-bind ((warning #'muffle-warning))
			  (cxml:make-catalog))))
    (let+ (((&accessors-r/o (scheme   puri:uri-scheme)
			    (path     puri:uri-path)
			    (fragment puri:uri-fragment)) (puri:uri url))
	   ((&values content code)
	    (ecase scheme
	      (:http (drakma:http-request url :want-stream t))
	      (:file (if (probe-file path)
			 (values (open path :element-type '(unsigned-byte 8)) 200)
			 (values nil 404))))))
      (cond
	((/= code 200)
	 (warn "~@<Broken link: ~
		~/more-conditions::print-reference/ => ~D~@:>"
	       spec code))

	(fragment
	 (let+ (((&flet resolver (pubid sysid)
		   (declare (ignore pubid))
		   (when (eq (puri:uri-scheme sysid) :http)
		     (drakma:http-request sysid :want-stream t))))
		(dom (cxml:parse content (stp:make-builder)
				 ;; :entity-resolver #'resolver
				 )))
	   (when (xpath:node-set-empty-p
		  (xpath:evaluate (format nil "//node()[@id='~A']" fragment) dom))
	     (warn "~@<Broken link anchor: ~/more-conditions::print-reference/: ~
		    attribute id='~A' not in document~@:>"
		   spec fragment))))))))

(defun collect+check-references (system
				 &optional
				 (preload (mapcar (curry #'asdf:system-relative-pathname system)
						  (list "src/package.lisp"
							"src/variables.lisp"
							"src/error-handling.lisp"))))
  "TODO(jmoringe): document"
  (mapc #'check-reference (collect-references system preload)))

(sb-ext:save-lisp-and-die "check-documentation-references"
			  :executable t
			  :toplevel   (lambda ()
					(collect+check-references :cl-rsb)
					(handler-case
					    (collect+check-references :cl-rsb)
					  (error (condition)
					    (format *error-output* "~A~%" condition)))))
