;;;; rsb-protocol-config.asd --- Configuration of RSB protocol files.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-protocol-config"
  :description "Configuration of RSB protocol files."
  :depends-on ("rsb-config")) ; for copy-file

(defmethod perform ((operation compile-op)
                    (component (eql (find-system "rsb-protocol-config"))))
  (let ((copy (ensure-function (string '#:copy-file) :package '#:rsb-config)))
    (map nil copy
         (input-files operation component)
         (output-files operation component))))

(flet ((protocol-directory ()
         (or (when (boundp 'cl-user::*rsb-protocol-directory*)
               (symbol-value 'cl-user::*rsb-protocol-directory*))
             (uiop:getenv "RSB_PROTOCOL_DIRECTORY"))))

  (defmethod input-files ((operation compile-op)
                          (component (eql (find-system "rsb-protocol-config"))))
    (let ((protocol-directory (protocol-directory)))
      (when protocol-directory
        (directory (merge-pathnames "**/*.proto" protocol-directory)))))

  (defmethod output-files ((operation compile-op)
                           (component (eql (find-system "rsb-protocol-config"))))
    (let ((output-directory   (system-relative-pathname component "data/"))
          (protocol-directory (protocol-directory)))
      (when protocol-directory
        (flet ((translate (input-file)
                 (merge-pathnames
                  (enough-namestring input-file protocol-directory)
                  output-directory)))
          (values (mapcar #'translate (input-files operation component)) t))))))
