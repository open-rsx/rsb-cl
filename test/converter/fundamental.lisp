;;;; fundamental.lisp --- Unit tests for "fundamental" converters.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter.test)

(def-suite fundamental-root
  :in converter-root
  :description
  "Root unit test suite for fundamental converters.")
(in-suite fundamental-root)

;;; Converter fundamental-void

(def-suite fundamental-void-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-void' converter.")
(in-suite fundamental-void-root)

(define-basic-converter-test-cases (:fundamental-void)
    `((,(octetify #())  :void ,+no-value+)
      (,(octetify #(1)) :void :not-applicable)
      (:not-applicable  :void t)
      (:not-applicable  :void ,(octetify #()))
      (:not-applicable  :void ,(octetify #(1)))))

;;; Converter fundamental-null

(def-suite fundamental-null-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-null' converter.")
(in-suite fundamental-null-root)

(define-basic-converter-test-cases (:fundamental-null)
    `((nil                     t nil)
      (:foo                    t :foo)
      ("bar"                   t "bar")
      (,(octetify #(65 65 65)) t ,(octetify #(65 65 65)))
      (,(octetify #(65 65 65)) t ,(octetify #(65 65 65)))))

;;; Converter fundamental-bool

(def-suite fundamental-bool-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-bool' converter.")
(in-suite fundamental-bool-root)

(define-basic-converter-test-cases (:fundamental-bool)
    `((,(octetify #(0)) :bool nil)
      (,(octetify #(1)) :bool t)
      (:not-applicable  :bool "bar")
      (,(octetify #(2)) :bool :error)))

;;; Converter fundamental-int32

(def-suite fundamental-int32-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-int32' converter.")
(in-suite fundamental-int32-root)

(define-basic-converter-test-cases (:fundamental-int32)
    `((,(octetify #(0 0 0 0))         :int32 0)
      (,(octetify #(1 0 0 0))         :int32 1)
      (,(octetify #(255 255 255 255)) :int32 -1)
      (:not-applicable                :int32 "bar")
      (,(octetify #(1))               :int32 :error)))

;;; Converter fundamental-ascii-string

(def-suite fundamental-ascii-string-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-ascii-string' converter.")
(in-suite fundamental-ascii-string-root)

(define-basic-converter-test-cases (:fundamental-ascii-string)
    `((,(octetify #())         :ascii-string "")
      (:not-applicable         :ascii-string :not-a-string)
      (,(octetify #(65 65 65)) :ascii-string "AAA")
      (:error                  :ascii-string "Aλ")
      (,(octetify #(65 129))   :ascii-string :error)))

;;; Converter fundamental-utf-8-string

(def-suite fundamental-utf-8-string-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-utf-8-string' converter.")
(in-suite fundamental-utf-8-string-root)

(define-basic-converter-test-cases (:fundamental-utf-8-string)
    `((,(octetify #())           :utf-8-string "")
      (:not-applicable           :utf-8-string :not-a-string)
      (,(octetify #(65 65 65))   :utf-8-string "AAA")
      (,(octetify #(65 206 187)) :utf-8-string "Aλ")
      (,(octetify #(255))        :utf-8-string :error)))

;;; Converter fundamental-bytes

(def-suite fundamental-bytes-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-bytes' converter.")
(in-suite fundamental-bytes-root)

(define-basic-converter-test-cases (:fundamental-bytes)
    `((,(octetify #())         :bytes ,(octetify #()))
      (:not-applicable         :bytes :not-an-octet-vector)
      (,(octetify #(65 65 65)) :bytes ,(octetify #(65 65 65)))))

;;; Converter fundamental-scope

(def-suite fundamental-scope-root
  :in fundamental-root
  :description
  "Unit tests for the `fundamental-scope' converter.")
(in-suite fundamental-scope-root)

(define-basic-converter-test-cases (:fundamental-scope
                                    :domain-test scope=)
    `((,(octetify #())                  :scope :error)
      (,(octetify #(102 111 111))       :scope :error)
      (:not-applicable                  :scope :not-a-scope)
      (,(octetify #(47))                :scope ,(make-scope "/"))
      (,(octetify #(47 102 111 111 47)) :scope ,(make-scope "/foo"))))

;; Local Variables:
;; coding: utf-8
;; End:
