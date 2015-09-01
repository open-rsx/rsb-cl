(cl:in-package #:puri)

(defun parse-uri (thing &key (class 'uri) &aux escape)
  (when (uri-p thing) (return-from parse-uri thing))

  (setq escape (escape-p thing))
  (multiple-value-bind (scheme host port path query fragment)
      (parse-uri-string thing)
    (when scheme
      (setq scheme
            (intern (funcall
                     (case *current-case-mode*
                       ((:case-insensitive-upper :case-sensitive-upper)
                        #'string-upcase)
                       ((:case-insensitive-lower :case-sensitive-lower)
                        #'string-downcase))
                     (decode-escaped-encoding scheme escape))
                    (find-package :keyword))))

    (when (and scheme (eq :urn scheme))
      (return-from parse-uri
        (make-instance 'urn :scheme scheme :nid host :nss path)))

    (when host (setq host (decode-escaped-encoding host escape)))
    (when port
      (setq port (read-from-string port))
      (cond
        ((not (numberp port))
         (error "port is not a number: ~s." port))
        ((minusp port)
         (error "port is not non-negative integer: ~d." port)))
      (when (eql port (case scheme
                        (:http 80)
                        (:https 443)
                        (:ftp 21)
                        (:telnet 23)))
        (setq port nil)))
    (when (or (string= "" path)
              (and ;; we canonicalize away a reference to just /:
               scheme
               (member scheme '(:http :https :ftp) :test #'eq)
               (string= "/" path)))
      (setq path nil))
    (when path
      (setq path
            (decode-escaped-encoding path escape *reserved-path-characters*)))
    (when query (setq query (decode-escaped-encoding query escape)))
    (when fragment
      (setq fragment
            (decode-escaped-encoding fragment escape
                                     *reserved-fragment-characters*)))
    (if* (eq 'uri class)
         then ;; allow the compiler to optimize the make-instance call:
         (make-instance 'uri
                        :scheme scheme
                        :host host
                        :port port
                        :path path
                        :query query
                        :fragment fragment
                        :escaped escape)
         else ;; do it the slow way:
         (make-instance class
                        :scheme scheme
                        :host host
                        :port port
                        :path path
                        :query query
                        :fragment fragment
                        :escaped escape))))
