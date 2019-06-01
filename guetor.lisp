;;;; guetor.lisp

(defpackage :guetor
  (:use :cl)
  (:export #:*selector*
           #:*mode*
           #:document
           #:available-elements
           #:element-selector
           #:selector))
(in-package :guetor)

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
                   (otherwise (warn "Unknown mode.")))))
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
          finally (return (element-selector result)))))

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
