;;;; guetor/interface.lisp

(uiop:define-package :guetor/interface (:nicknames :guetor)
  (:use :cl)
  (:mix :guetor/content)
  (:export #:*selector*
           #:*mode*
           #:document
           #:available-elements
           #:element-selector
           #:selector
           #:contents-wrapper))
