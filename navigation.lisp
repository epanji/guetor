;;;; guetor/navigation.lisp

(defpackage :guetor/navigation
  (:nicknames :guetor-nav)
  (:use :cl :guetor/condition)
  (:import-from :lquery)
  (:export #:*standard-domain-text*
           #:*standard-forward-text*
           #:*standard-backward-text*
           #:*navigation-selector*
           #:*navigation-base*
           #:*navigation-index*
           #:*navigation-direction*
           #:prepare-navigation-text
           #:navigation-text
           #:navigation-text-equal
           #:complete-navigation-href
           #:find-navigation-node
           #:find-navigation
           #:navigation-not-found
           #:navigation-base-unset))

(in-package :guetor/navigation)

;;; This navigation only have forward and backward.

(defparameter *standard-domain-text* "http://domain.com")
(defparameter *standard-forward-text* "Next")
(defparameter *standard-backward-text* "Prev")

(defparameter *navigation-selector* "link, a")
(defparameter *navigation-base* *standard-domain-text*)
(declaim (string *standard-domain-text*
                 *standard-forward-text*
                 *standard-backward-text*
                 *navigation-selector*
                 *navigation-base*))

(defparameter *navigation-index* nil
  "Index for multiple navigation with the same text.
Set to NIL will make navigation choose last index by default.")
(declaim (type (or integer null) *navigation-index*))

(defparameter *navigation-direction* :forward
  "Direction for navigation with optional :FORWARD or :BACKWARD.")
(declaim (type (member :forward :backward) *navigation-direction*))

(defun prepare-navigation-text (&key
                                  (base nil d-p)
                                  (forward nil f-p)
                                  (backward nil b-p))
  (when d-p (setf *navigation-base* base))
  (when f-p (setf *standard-forward-text* forward))
  (when b-p (setf *standard-backward-text* backward))
  (list *navigation-base* *standard-forward-text* *standard-backward-text*))

(defun navigation-text ()
  (ecase *navigation-direction*
    (:forward *standard-forward-text*)
    (:backward *standard-backward-text*)))

(defun navigation-text-equal (text1 text2)
  (flet ((trim-text (text)
           (remove-if (complement #'alpha-char-p) text)))
    (let ((t1 (trim-text text1))
          (t2 (trim-text text2)))
      (string-equal t1 t2))))

;;; Navigation element / Html link.

(defun complete-navigation-href (string)
  (cond ((null string)
         (error 'navigation-not-found
                :message (format nil "No navigation ~(~A~) with text ~S.~%"
                                 (string *navigation-direction*)
                                 (navigation-text))))
        ((string-equal (subseq string 0 (min 4 (length string)))
                       (subseq *standard-domain-text* 0 4))
         string)
        ((string= *navigation-base* *standard-domain-text*)
         (error 'navigation-base-unset
                :message (format nil "Variable *navigation-base* is ~
                                 not set. Use prepare-navigation-text ~
                                 to set it up.~%")))
        (t ; Ensure having single slash separator
         (concatenate 'string (string-trim "/" *navigation-base*) "/"
                      (string-trim "/" string)))))

(defun find-navigation-node (working-nodes)
  (lquery:$ working-nodes *navigation-selector*
    (filter (lambda (node)
              (when (navigation-text-equal
                     (or (lquery-funcs:attr node :rel)
                         (lquery-funcs:render-text node))
                     (navigation-text))
                node)))))

(defun find-navigation (working-nodes &optional direction)
  (let* ((*navigation-direction* (or direction *navigation-direction*))
         (node (find-navigation-node (lquery-funcs:root working-nodes)))
         (index (or *navigation-index* (max 0 (1- (length node))))))
    (lquery-funcs:attr node :href)
    (complete-navigation-href (lquery-funcs:node node index))))

;;; Conditions

(define-condition navigation-not-found (guetor-error) ())
(define-condition navigation-base-unset (guetor-error) ())
