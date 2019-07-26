;;;; guetor/title.lisp

(defpackage :guetor/title
  (:nicknames :guetor-t)
  (:use :cl)
  (:import-from :lquery)
  (:export #:*header-selector*
           #:*header-position*
           #:has-title-p
           #:title))
(in-package :guetor/title)

(defparameter *header-selector* "h1, h2, h3, h4, h5, title")
(defparameter *header-position* 0)



(defun has-title-p (node)
  (plusp (length (clss:select *header-selector* node))))

(defun find-header (node &optional fn)
  (loop for result = (clss:select *header-selector* node)
        until (or (plump:root-p node)
                  (plusp (length result))
                  (zerop (plump:element-position node)))
        do (setf node (funcall (or fn 'plump:previous-element) node))
        finally (return result)))



(defun title (node &optional (key :element) position selector)
  "
Guess title from NODE contents. If none inside NODE, it will search up until reach top document.

Arguments to this function are as follow:

KEY:      a keyword with 4 options. (:ELEMENT :TEXT :ELEMENT-VECTOR :TEXT-VECTOR)
POSITION: a positive integer as position inside vector, ignored if KEY equal *-VECTOR.
SELECTOR: a string like CSS selector for header.

This function will return the TITLE and PREDICATE for node having title.
"
  (let ((*header-position* (or position *header-position*))
        (*header-selector* (or selector *header-selector*)))
    (values (guess-title node key) (has-title-p node))))

(defgeneric guess-title (node key)
  (:method (node key))
  (:documentation "Guess title from PLUMP-DOM:ELEMENT contents."))

(defmethod guess-title ((node plump-dom:root) key)
  (guess-title (find-header node) key))

(defmethod guess-title ((node plump-dom:element) key)
  (guess-title (find-header node 'plump:parent) key))

(defmethod guess-title ((node vector) (key (eql :element-vector)))
  node)

(defmethod guess-title ((node vector) (key (eql :text-vector)))
  (lquery-funcs:text node))

(defmethod guess-title ((node vector) (key (eql :element)))
  (when (and (plusp (length node))
             (< *header-position* (length node)))
    (aref node *header-position*)))

(defmethod guess-title ((node vector) (key (eql :text)))
  (lquery-funcs:text (or (guess-title node :element) (make-string 0))))
