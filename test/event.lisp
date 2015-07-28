;;;; event.lisp --- Unit tests for the event class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite event-root (root)
  ()
  (:documentation
   "Unit tests for the `event' class."))

(addtest (event-root
          :documentation
          "Test construction of `event' instances.")
  construction

  (ensure-cases (initargs expected)
      `(;; Cannot have an `event' without scope
        (() missing-required-initarg)

        ;; These are OK
        ((:scope ,(make-scope "/") :data "foo") ("/"        "foo" (:create)))
        ((:scope "/"               :data "foo") ("/"        "foo" (:create)))
        ((:scope ("foo" "bar")     :data "foo") ("/foo/bar" "foo" (:create)))

        ((:scope "/baz"            :data "bar" :create-timestamp? nil)
         ("/baz/" "bar" ((not :create))))

        ((:scope "/baz"            :data "bar" :unchecked? t)
         ("/baz" "bar" (:create)))

        ((:scope "/"               :data "foo" :timestamps (:foo ,(local-time:now)))
         ("/" "foo" (:create :foo))))

    (let+ (((&flet via-make-instance ()
              (apply #'make-instance 'event initargs)))
           ((&flet via-make-event (scope data initargs)
              (apply #'make-event scope data initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition 'missing-required-initarg
           (via-make-instance)))
        (t
         (let+ (((expected-scope expected-payload expected-timestamps)
                 expected)
                ((&flet check (event)
                   (check-event event expected-scope expected-payload)
                   (dolist (expected-timestamp expected-timestamps)
                     (typecase expected-timestamp
                       (keyword
                        (ensure (typep (timestamp event expected-timestamp)
                                       'local-time:timestamp)))
                       ((cons (eql not) (cons keyword null))
                        (ensure-null
                         (timestamp event (second expected-timestamp)))))))))
           (unless (getf initargs :unchecked?)
             (check (via-make-instance)))
           (when-let ((scope (getf initargs :scope))
                      (data  (getf initargs :data)))
             (check
              (via-make-event
               scope data (remove-from-plist initargs :scope :data)))))))))

  ;; Cannot have an `event' without scope
  (ensure-condition 'missing-required-initarg
    (make-instance 'event))

  (let ((event (make-instance 'event
                              :scope (make-scope "/")
                              :data  "foo")))
    (check-event event "/" "foo")
    (ensure (typep (timestamp event :create) 'local-time:timestamp)))

  (let ((event (make-instance 'event
                              :scope             (make-scope "/baz")
                              :data              "bar"
                              :create-timestamp? nil)))
    (check-event event "/baz/" "bar")
    (ensure-null (timestamp event :create)))

  (let ((event (make-instance 'event
                              :scope (make-scope "/")
                              :data  "foo"
                              :timestamps `(:foo ,(local-time:now)))))
    (check-event event "/" "foo")
    (ensure (typep (timestamp event :foo) 'local-time:timestamp))))

(addtest (event-root
          :documentation
          "Test computation of event sequence numbers.")
  id-computation

  ;; Test some examples. Note that event ids cannot be computed
  ;; without origin id.
  (ensure-cases (sequence-number origin expected)
      `((0 nil nil)
        (0
         ,(make-id "D8FBFEF4-4EB0-4C89-9716-C425DED3C527")
         ,(make-id "84F43861-433F-5253-AFBB-A613A5E04D71"))
        (378
         ,(make-id "BF948D47-618F-4B04-AAC5-0AB5A1A79267")
         ,(make-id "BD27BE7D-87DE-5336-BECA-44FC60DE46A0")))
    (let ((event (apply #'make-instance 'event
                        :sequence-number sequence-number
                        :scope           (make-scope "/")
                        (when origin
                          (list :origin origin)))))
      (if expected
          (ensure-same (event-id event) expected
                       :test #'uuid:uuid=)
          (ensure-null (event-id event))))))

(addtest (event-root
          :documentation
          "Test comparing events for equality using `event='.")
  comparison

  (ensure-cases (scope1 data1 origin1?
                 scope2 data2 origin2?
                 =1? =2? =3? =4? =5?)
      '(("/"    "bar" nil     "/" "bar" nil     t   nil t   nil t)
        ("/"    "bar" t       "/" "bar" nil     t   nil nil nil t)
        ("/"    "bar" t       "/" "bar" t       t   nil t   nil t)
        ("/foo" "bar" nil     "/" "bar" nil     nil nil nil nil nil)
        ("/"    "baz" nil     "/" "bar" nil     nil nil nil nil nil))

    (let* ((origin (uuid:make-v1-uuid))
           (left   (make-event scope1 data1))
           (right  (progn
                     (sleep .000001) ;; force different create times
                     (make-event scope2 data2)) ))

      (setf (event-sequence-number left) 0)
      (when origin1?
        (setf (event-origin left) origin))

      (setf (event-sequence-number right) 1)
      (when origin2?
        (setf (event-origin right) origin))

      (iter (for (sequence-numbers? origins? timestamps? causes? expected)
                 in `((nil nil nil nil ,=1?)
                      (t   nil nil nil ,=2?)
                      (nil t   nil nil ,=3?)
                      (nil nil t   nil ,=4?)
                      (nil nil nil t   ,=5?)))
            (ensure-same
             (event= left right
                     :compare-sequence-numbers? sequence-numbers?
                     :compare-origins?          origins?
                     :compare-timestamps        timestamps?
                     :compare-causes?           causes?
                     :data-test                 #'equal)
             expected
             :report    "~@<When compared ~:[without~;with~] sequence ~
                         numbers, ~:[without~;with~] origins, ~
                         ~:[without~;with~] timestamps and ~
                         ~:[without~;with~] causes, events ~A and ~A were ~
                         ~:[not ~;~]equal, but expected ~:[not ~;~]to ~
                         be.~@:>"
             :arguments (sequence-numbers? origins? timestamps? causes?
                         left right (not expected) expected))))))

(addtest (event-root
          :documentation
          "Test `print-object' method on `event' class.")
  print

  (ensure-cases (args expected)
      `((("/foo/bar" "baz")
         "/foo/bar/ \"baz\" (3)")
        (("/foo/bar" ,(make-string 1000 :initial-element #\a))
         "/foo/bar/ \"aaaaaaaaaa...\" (1000)")
        (("/foo/bar" "with
newline")
         "/foo/bar/ \"with.newli...\" (12)")
        (("/" ,(make-scope "/foo/bar/"))
         "/ /foo/bar/"))
    (let+ ((event  (apply #'make-event args))
           (string (let ((*print-length* 10))
                     (princ-to-string event)))
           ((&flet search/flipped (string expected)
              (search expected string))))
      (ensure-same string expected :test #'search/flipped))))
