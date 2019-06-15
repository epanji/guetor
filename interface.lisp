;;;; guetor/interface.lisp

(uiop:define-package :guetor/interface (:nicknames :guetor)
  (:use :cl)
  (:mix :guetor/content :guetor/content-markless)
  (:export #:*selector*
           #:*mode*
           #:document
           #:available-elements
           #:element-selector
           #:selector
           #:contents-wrapper
           #:output-markless))
