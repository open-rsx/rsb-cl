;;;; rsb-version.asd --- Versioning of the rsb system.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "rsb-version"
  :description "Versioning of the rsb system."
  :depends-on  ("rsb-config"))

(defmethod perform ((operation compile-op)
                    (component (eql (find-system "rsb-version"))))
  (apply #'uiop:symbol-call '#:rsb-config '#:update-version-file
         (first (input-files operation component))
         (output-files operation component)))

(defmethod input-files ((operation compile-op)
                        (component (eql (find-system "rsb-version"))))
  ;; Regenerate version information if major or minor version in
  ;; version.sexp or the GIT index appear modified.
  (list* (system-relative-pathname component "version.sexp")
         (let ((index (merge-pathnames ".git/index"
                                       (component-pathname component))))
           (when (uiop:file-exists-p index)
             (list index)))))

(defmethod output-files ((operation compile-op)
                         (component (eql (find-system "rsb-version"))))
  (values `(#P"version-details.sexp" #P"version-string.sexp") t))
