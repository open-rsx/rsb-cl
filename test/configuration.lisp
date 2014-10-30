;;;; configuration.lisp --- Unit tests for configuration functions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite configuration-root (root)
  ()
  (:documentation
   "Unit tests for the `scope' class."))

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
