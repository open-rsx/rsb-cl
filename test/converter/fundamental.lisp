;;;; fundamental.lisp --- Unit tests for "fundamental" converters.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.converter.test)

(deftestsuite fundamental-root (converter-root)
  ()
  (:documentation
   "Root unit test suite for fundamental converters."))


;;; Converter fundamental-void
;;

(deftestsuite fundamental-void-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-void' converter."))

(define-basic-converter-test-cases (:fundamental-void)
    `((,(octetify #())  :void ,+no-value+)
      (,(octetify #(1)) :void :not-applicable)
      (:not-applicable  :void t)
      (:not-applicable  :void ,(octetify #()))
      (:not-applicable  :void ,(octetify #(1)))))


;;; Converter fundamental-null
;;

(deftestsuite fundamental-null-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-null' converter."))

(define-basic-converter-test-cases (:fundamental-null)
    `((nil                     t    nil)
      (:foo                    t    :foo)
      ("bar"                   t    "bar")
      (,(octetify #(65 65 65)) t    ,(octetify #(65 65 65)))
      (,(octetify #(65 65 65)) :foo ,(octetify #(65 65 65)))))


;;; Converter fundamental-bool
;;

(deftestsuite fundamental-bool-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-bool' converter."))

(define-basic-converter-test-cases (:fundamental-bool)
    `((,(octetify #(0)) :bool nil)
      (,(octetify #(1)) :bool t)
      (:not-applicable  :bool "bar")
      (,(octetify #(2)) :bool :error)))


;;; Converter fundamental-int32
;;

(deftestsuite fundamental-int32-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-int32' converter."))

(define-basic-converter-test-cases (:fundamental-int32)
    `((,(octetify #(0 0 0 0))         :int32 0)
      (,(octetify #(1 0 0 0))         :int32 1)
      (,(octetify #(255 255 255 255)) :int32 -1)
      (:not-applicable                :int32 "bar")
      (,(octetify #(1))               :int32 :error)))


;;; Converter fundamental-ascii-string
;;

(deftestsuite fundamental-ascii-string-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-ascii-string' converter."))

(define-basic-converter-test-cases (:fundamental-ascii-string)
    `((,(octetify #())         :ascii-string "")
      (:not-applicable         :ascii-string :not-a-string)
      (,(octetify #(65 65 65)) :ascii-string "AAA")
      (:error                  :ascii-string "Aλ")
      (,(octetify #(65 129))   :ascii-string :error)))


;;; Converter fundamental-utf-8-string
;;

(deftestsuite fundamental-utf-8-string-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-utf-8-string' converter."))

(define-basic-converter-test-cases (:fundamental-utf-8-string)
    `((,(octetify #())           :utf-8-string "")
      (:not-applicable           :utf-8-string :not-a-string)
      (,(octetify #(65 65 65))   :utf-8-string "AAA")
      (,(octetify #(65 206 187)) :utf-8-string "Aλ")
      (,(octetify #(255))        :utf-8-string :error)))


;;; Converter fundamental-bytes
;;

(deftestsuite fundamental-bytes-root (fundamental-root)
  ()
  (:documentation
   "Unit tests for the `fundamental-bytes' converter."))

(define-basic-converter-test-cases (:fundamental-bytes)
    `((,(octetify #())         :bytes ,(octetify #()))
      (:not-applicable         :bytes :not-an-octet-vector)
      (,(octetify #(65 65 65)) :bytes ,(octetify #(65 65 65)))))

;; Local Variables:
;; coding: utf-8
;; End:
