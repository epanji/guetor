;;;; guetor/content-markless.lisp

(defpackage :guetor/content-markless
  (:use :cl)
  (:import-from :plump *stream*)
  (:export #:output-markless))
(in-package :guetor/content-markless)

(defun markless-tag-begin (element)
  (let ((tag (plump:tag-name element)))
    (cond ((string= tag "h1") "# ")
          ((string= tag "h2") "## ")
          ((string= tag "h3") "### ")
          ((string= tag "h4") "#### ")
          ((string= tag "h5") "##### ")
          (t ""))))

(defun markless-tag-end (element)
  (let ((tag (plump:tag-name element)))
    (cond ((or (string= tag "h1")
               (string= tag "h2")
               (string= tag "h3")
               (string= tag "h4")
               (string= tag "h5")
               (string= tag "p"))
           (format nil "~3&"))
          (t ""))))

(defun output-markless (node &optional (stream t))
  (cond ((eql stream t)
         (let ((*stream* *standard-output*))
           (output-element node)))
        ((eql stream nil)
         (with-output-to-string (*stream*)
           (output-element node)))
        (t
         (let ((*stream* stream))
           (output-element node)))))

(defmethod output-element ((node plump:element))
  (write-string (markless-tag-begin node) *stream*)
  (when (< 0 (length (plump:children node)))
    (loop for child across (plump:children node)
          do (output-element child)))
  (write-string (markless-tag-end node) *stream*))

(defmethod output-element ((node plump:text-node))
  (plump:encode-entities (plump:render-text node) *stream*))
