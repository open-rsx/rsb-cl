;;;; examples.lisp --- Tests of example programs.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(define-constant +terminate-marker+
    "Loading this file does not terminate"
  :test #'string=)

(defun test-example (file &key (trace-stream *trace-output*))
  (format trace-stream "/// Begin example ~24,0T~S~%" file)
  (unwind-protect
       (let+ ((does-not-terminate?
               (search +terminate-marker+
                       (read-file-into-string file)))
              (stream (make-string-output-stream))
              ((&values output-file warnings? failure?)
               (let ((*standard-output* stream)
                     (*error-output*    stream))
                 (handler-bind ((warning #'muffle-warning))
                   (format trace-stream "/// Compiling example ~24,0T~S~%" file)
                   (with-standard-io-syntax
                     (let ((*print-readably* nil)) ; SBCL workaround
                       (with-compilation-unit (:override t)
                         (compile-file file))))))))
         (cond
           ((or warnings? failure?)
            (error "~@<Failed to compile example file ~S~@:_~@:_~
                    ~2@T~<~@;~A~:>~:>"
                   file (list (get-output-stream-string stream))))

           (does-not-terminate?
            (format trace-stream "/// Not loading example ~24,0T~S~%~
                                  /// ~24,0TThe file is marked to not ~
                                  terminate when loaded~%"
                    file))

           (t
            (handler-case
                (progn
                  (format trace-stream "/// Loading example ~24,0T~S~2%~2@T" file)
                  (pprint-logical-block (trace-stream (list) :per-line-prefix "> ")
                    (let ((*package*         (find-package '#:cl-user))
                          (*standard-output* trace-stream))
                      (load output-file))))
              (error (condition)
                (error "~@<Failed to load example file ~S~@:_~@:_~
                        ~2@T~<| ~@;~A~:>~
                        ~:>"
                       file (list condition))))
            (format trace-stream "~2%"))))
    (format trace-stream "~&/// End example ~24,0T~S~2%" file)))

(defun example-files ()
  (directory (merge-pathnames "**/*.lisp"
                              (asdf:system-relative-pathname
                               :cl-rsb "examples/"))))

(deftestsuite examples-root (root)
  ()
  (:timeout 60)
  (:documentation
   "Test suite for compiling and loading example files in the examples
    directory."))

(addtest (examples-root
          :documentation
          "Test compiling and loading example files in the examples
           directory.")
  compile-and-load

  (ensure-cases (file)
      (example-files)
    (test-example file)))
