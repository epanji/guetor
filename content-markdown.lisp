;;;; guetor/content-markdown.lisp

(defpackage :guetor/content-markdown
  (:use :cl)
  (:import-from :plump #:*stream*)
  (:import-from :guetor/output
                #:*collection-plist-tag*
                #:*plist-attribute*
                ;;----------------------
                #:*record-parents*
                #:*record-number*
                #:*record-string*
                ;;----------------------
                #:define-output)
  (:export #:output-markdown))
(in-package :guetor/content-markdown)

;;; TODO
(define-output output-markdown (nil nil))
