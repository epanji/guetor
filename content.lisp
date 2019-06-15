;;;; guetor/content.lisp

(defpackage :guetor/content
  (:use :cl)
  (:import-from :lquery)
  (:export #:*selector*
           #:*mode*
           #:document
           #:available-elements
           #:element-selector
           #:selector
           #:%perform-guess-p
           #:contents-wrapper))
(in-package :guetor/content)

(defparameter *document* nil
  "Root DOM node for temporary document.")

(defparameter *selector* nil
  "Current CSS selector to avoid re-guess.")

(defparameter *default-selector* "div"
  "Default selector for contents wrapper.")

(defparameter *default-tag-name* "p"
  "Default tag name for almost every content inside wrapper.")

(defparameter *mode* :simple
  "Mode for selector, :SIMPLE or :FULL.")

(defparameter *content* nil
  "Element node for contents wrapper.")

(defun document (input)
  (unless (null input) (plump:parse input)))

(defun %element-selector (element)
  (when (typep element 'plump-dom:element)
    (let* ((tag (plump:tag-name element))
           (attrs (plump:attributes element))
           (id (gethash "id" attrs))
           (class (gethash "class" attrs)))
      (format nil "~A~@[#~A~]~@[.~A~]" tag id
              (substitute #\. #\Space class)))))

(defun element-selector (element)
  (let ((current (%element-selector element))
        (parents (case *mode*
                   (:simple ())
                   (:full (map 'list
                               '%element-selector
                               (lquery-funcs:parents element)))
                   (otherwise (warn "Unknown mode: ~A" *mode*)))))
    (format nil "~@[~{~A ~}~]~A"
            (reverse parents)
            (or current *default-selector*))))

(defun available-elements (document)
  (map 'list 'identity
       (lquery:$ document *default-tag-name* (parent))))

(defun %selector (document)
  (when (plump-dom:root-p document)
    (loop with all = (available-elements document)
          and last = 0
          and result = nil
          with no-dups = (remove-duplicates all)
          for item in no-dups
          for tmp = (count item all)
          when (> tmp last)
            do (setf last tmp result item)
          finally (setf *content* result)
                  (return (element-selector result)))))

(defun %perform-guess-p ()
  (or (null *selector*)
      (not (stringp *selector*))
      (string= *default-selector* *selector*)))

(defun selector (&optional document force-p)
  (let ((root (or (document document) *document*)))
    (cond ((null root) *default-selector*)
          ((or (%perform-guess-p) force-p)
           (progn (unless (eql *document* root) (setf *document* root))
                  (setf *selector* (or (%selector root) *default-selector*))))
          (t *selector*))))

(defun contents-wrapper (document &optional force-p)
  (let ((guess-p (or (%perform-guess-p) force-p))
        (selector (selector document force-p))
        (root (document document)))
    (if guess-p
        (values *content* guess-p)
        (loop with elements = (lquery:$ root selector)
              and size = -1             ; In case current = 0
              and target = nil
              for element across elements
              for current = (length (lquery-funcs:children
                                     element *default-tag-name*))
              when (> current size)
                do (setf size current target element)
              finally (return (values target guess-p))))))
