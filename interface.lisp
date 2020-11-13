;;;; guetor/interface.lisp

(uiop:define-package :guetor/interface (:nicknames :guetor)
  (:use :cl)
  (:mix
   :guetor/condition
   :guetor/content
   :guetor/title
   :guetor/output
   :guetor/navigation
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
   ;; guetor/navigation
   #:*navigation-base*
   #:*navigation-index*
   #:*navigation-direction*
   #:prepare-navigation-text
   #:find-navigation
   #:navigation-not-found
   #:navigation-base-unset
   ;; guetor/output
   #:*skipped-texts*
   ;; guetor/content-markless
   #:output-markless
   ;; guetor/content-markdown
   #:output-markdown))
