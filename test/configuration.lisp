;;;; configuration.lisp --- Unit tests for configuration functions.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite configuration-root (root)
  ()
  (:documentation
   "Unit tests for configuration functions."))

(addtest (configuration-root
          :documentation
          "Smoke test for the `transport-options'.")
  transport-options/smoke

  (ensure-cases (options expected)
      '(;; Empty and not transport-related.
        (()                                                   ())
        ((((:foo) . 1))                                       ())
        ((((:foo :bar) . 1))                                  ())
        ;; Basic Transport options.
        ((((:transport :foo :bar) . 1))                       ((:foo . (:bar 1))))
        ((((:transport :foo :bar) . 1)
          ((:transport :foo :fez) . 2))                       ((:foo . (:bar 1 :fez 2))))
        ((((:transport :foo :bar)   . 1)
          ((:transport :fez :whoop) . 2))                     ((:foo . (:bar 1))
                                                               (:fez . (:whoop 2))))
        ;; Converter options.
        ((((:transport :foo :converters :lisp :foo) . "bar")) ((:foo . ())))
        ;; Unrelated converter options.
        ((((:transport :foo :converters :cpp :foo) . "bar"))  ((:foo . ()))))
    (ensure-same (rsb::transport-options options) expected :test #'equal)))

(addtest (configuration-root
          :documentation
          "Smoke test for the (internal) `effective-transport-options'
           function.")
  effective-transport-options/smoke

  (ensure-cases (options expected)
    '(;; Nothing.
      (()                                                         ())
      ;; Some transport.
      (((:socket :enabled nil))                                   ())
      (((:socket :enabled nil :port 1))                           ())
      (((:socket :enabled t))                                     ((:socket)))
      (((:socket :enabled t :port 1))                             ((:socket :port 1)))
      ;; Only defaults.
      (((t :enabled nil))                                         ())
      (((t :enabled nil :port 1))                                 ())
      (((t :enabled t))                                           ())
      (((t :enabled t :port 1))                                   ())
      ;; Transport and defaults without inheritance.
      (((:socket)                               (t :enabled t))   ())
      (((:socket :port 1)                       (t :enabled t))   ())
      (((:socket)                               (t :enabled nil)) ())
      (((:socket :port 1)                       (t :enabled nil)) ())
      (((:socket :enabled nil)                  (t :enabled t))   ())
      (((:socket :enabled nil :port 1)          (t :enabled t))   ())
      (((:socket :enabled t)                    (t :enabled t))   ((:socket)))
      (((:socket :enabled t :port 1)            (t :enabled t))   ((:socket :port 1)))
      (((:socket :enabled nil)                  (t :enabled nil)) ())
      (((:socket :enabled nil :port 1)          (t :enabled nil)) ())
      (((:socket :enabled t)                    (t :enabled nil)) ((:socket)))
      (((:socket :enabled t :port 1)            (t :enabled nil)) ((:socket :port 1)))
      ;; Transport and defaults with inheritance.
      (((:socket &inherit)                      (t :enabled t))   ())
      (((:socket :port 1 &inherit)              (t :enabled t))   ())
      (((:socket &inherit)                      (t :enabled nil)) ())
      (((:socket :port 1 &inherit)              (t :enabled nil)) ())
      (((:socket :enabled nil &inherit)         (t :enabled t))   ())
      (((:socket :enabled nil :port 1 &inherit) (t :enabled t))   ())
      (((:socket :enabled t &inherit)           (t :enabled t))   ((:socket)))
      (((:socket :enabled t :port 1 &inherit)   (t :enabled t))   ((:socket :port 1)))
      (((:socket :enabled nil &inherit)         (t :enabled nil)) ())
      (((:socket :enabled nil :port 1 &inherit) (t :enabled nil)) ())
      (((:socket :enabled t &inherit)           (t :enabled nil)) ((:socket)))
      (((:socket :enabled t :port 1 &inherit)   (t :enabled nil)) ((:socket :port 1))))

    (let ((original (copy-tree options))
          (result   (rsb::effective-transport-options options)))
      (ensure-same result expected :test #'equal)
      (ensure-same options original :test #'equal))))

(addtest (configuration-root
          :documentation
          "Smoke test for the (internal) `merge-transport-options'
           function.")
  merge-transport-options/smoke

  (ensure-cases (left right expected)
      '(;; Nothing
        (()
         ()
         ())
        ;; Transport without defaults.
        (((:socket :port 1))
         ()
         ((:socket :port 1)))
        ;; Defaults without transport.
        (()
         ((:socket :port 1))
         ((:socket :port 1)))
        ;; Transport and defaults.
        (((:socket :port 1))
         ((:socket :host "foo"))
         ((:socket :port 1)))
        ;; Transport with inheritance but no defaults.
        (((:socket :port 1 &inherit) (t :host "foo"))
         ()
         ((:socket :port 1 :host "foo") (t :host "foo")))
        ;; Transport with inheritance and defaults.
        (((:socket :port 1 &inherit))
         ((:socket :host "foo"))
         ((:socket :port 1 :host "foo"))))

    (let ((original-left  (copy-tree left))
          (original-right (copy-tree right))
          (result         (rsb::merge-transport-options left right)))
      (ensure-same result expected :test #'equal)
      (ensure-same left  original-left  :test #'equal)
      (ensure-same right original-right :test #'equal))))
