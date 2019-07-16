;;;; guetor/interface.lisp

(uiop:define-package :guetor/interface (:nicknames :guetor)
  (:use :cl)
  (:mix
   :guetor/content
   :guetor/title
   :guetor/output
   :guetor/content-markless
   :guetor/content-markdown)
  (:export
   ;; guetor/content
   #:*mode*
   #:*content*
   #:*document*
   #:*selector*
   #:document
   #:available-elements
   #:element-selector
   #:selector
   #:contents-wrapper
   ;; guetor/title
   #:has-title-p
   #:title
   ;; guetor/content-markless
   #:output-markless
   ;; guetor/content-markdown
   #:output-markdown))
