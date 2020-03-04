;;;; guetor/output.lisp
;;;;
;;;; How to extends this system?
;;;;
;;;; By use-package or manually import-from this system:
;;;;
;;;; (:import-from :plump #:*stream*)
;;;; (:import-from :guetor/output
;;;;               #:*collection-plist-tag*
;;;;               #:*plist-attribute*
;;;;               #:*record-parents*
;;;;               #:*record-number*
;;;;               #:*record-string*
;;;;               #:define-output)
;;;;
;;;; After that, define-output for new system.

(defpackage :guetor/output
  (:nicknames :guetor-o)
  (:use :cl)
  (:import-from :plump #:*stream*)
  (:import-from :lquery)
  (:export #:*collection-plist-tag*
           #:*plist-attribute*
           ;; ---------------------
           #:*record-parents*
           #:*record-number*
           #:*record-string*
           ;; ---------------------
           #:define-output))
(in-package :guetor/output)


;;; Tag open and close rules:
;;; - Forbid space or end of line on first character opening tag.
;;; - May have end of line but not space on last character closing tag.
;;; - Tag that affect multiple tags have to set line prefix.
;;; - Line prefix can not affect tag with pure-tag-p value T.
;;; - Line prefix may become indent except tag with absolute-prefix-p value T.
;;; - Tag with style-tag-p value T will be check agains punctuations.
;;; - Tag may have indicator predicate for skip-children-p.
;;; - Avoid trim with raw-children-text-p value T.
;;; - Undefine indicator means the value NIL or Empty.
;;; - Tag with different output may have alt-open and alt-close.
;;; - All indicator predicate does not apply for alternative.
;;; - Use swap-attributes-p value T for attributes location after text.
;;;
;;; Property list indicator and value
;;; - :name "TAG-NAME"
;;; - :open "OPEN-TAG"
;;; - :close "CLOSE-TAG"
;;; - :alt-open "ALTERNATIVE"
;;; - :alt-close "ALTERNATIVE"
;;; - :prefix "LINE-PREFIX"
;;; - :absolute-prefix-p NIL
;;; - :pure-tag-p NIL
;;; - :style-tag-p NIL
;;; - :skip-children-p NIL
;;; - :raw-children-text-p NIL
;;; - :swap-attributes-p NIL
(defvar *collection-plist-tag* nil)

;;; Format control for attribute value
;;; - :src "~A"
;;; - :data-language "~A~&"
(defvar *plist-attribute* nil)

(defun indicator-value (plist &optional (indicator :name) (default ""))
  (getf plist indicator default))

(defun indicator-values (indicator predicate)
  (remove-duplicates (loop for tag in *collection-plist-tag*
                           for value = (indicator-value tag indicator nil)
                           when (and (plusp (length value))
                                     (indicator-value tag predicate nil))
                             collect value)))

(defun plist-tag (name)
  (find name *collection-plist-tag* :test 'string= :key 'indicator-value))

(defun plist-tag-value (name indicator &optional (default ""))
  (let ((plist-tag (plist-tag name)))
    (indicator-value plist-tag indicator default)))

(defun skip-children-p (node)
  (or (zerop (length (plump:children node)))
      (plist-tag-value (plump:tag-name node) :skip-children-p nil)))

(defun remove-digit (string)
  (remove-if 'digit-char-p string))

(defun tail-string (string &optional (max 2))
  (subseq string (max 0 (- (length string) max))))

(defun concatenate-keyword (&rest inputs)
  (intern (string-upcase
           (apply 'concatenate 'string
                  (mapcar 'string inputs)))
          :keyword))

(defun attribute-format (name)
  (indicator-value *plist-attribute* (concatenate-keyword name) nil))


;;; Counter
;;;
(defvar +reset-number+ 0)
(defvar +delta-number+ 1)

(defvar *record-number* +reset-number+)

(defun counter (stream arg &optional colon at-sign)
  (declare (ignore arg colon at-sign))
  (princ *record-number* stream))

(defun update-counter (node)
  (cond ((string= (plump:tag-name node) "ol") +reset-number+)
        ((string= (plump:tag-name node) "li")
         (incf *record-number* +delta-number+))
        (t *record-number*)))

;;; Spacer
;;;
(defvar +empty-string+ (make-string 0))
(defvar +space-string+ (make-string 1 :initial-element #\Space))

(defvar *record-string* +empty-string+)

(defun spacer (stream arg &optional colon at-sign)
  (declare (ignore arg colon at-sign))
  (princ (if (or (zerop (length *record-string*))
                 (string= *record-string* "~&")
                 (string= *record-string* "~%")
                 (find #\Space *record-string*))
             +empty-string+
             +space-string+)
         stream))

(defun update-spacer (string)
  (let* ((no-digit (remove-digit string))
         (length (length no-digit)))
    (when (plusp length)
      (setf *record-string* (tail-string no-digit))))
  string)

;;; Prefixer
;;;
(defvar *record-parents* nil)

(defun prefixer ()
  (apply
   'concatenate
   'string
   (when (plusp (length *record-parents*))
     (reverse
      (loop with line-p = (or (string= *record-string* "~&")
                              (string= *record-string* "~%"))
            and first-p = t
            for name in *record-parents*
            for prefix = (plist-tag-value name :prefix "")
            and absolute-prefix-p = (plist-tag-value
                                     name :absolute-prefix-p nil)
            collect (if (or absolute-prefix-p first-p)
                        (progn (setf first-p nil) (string prefix))
                        (if line-p
                            (make-string
                             (length (apply 'format nil prefix
                                            (make-list 4 :initial-element
                                                       +empty-string+)))
                             :initial-element #\Space)
                            +empty-string+)))))))

(defun update-prefixer (node)
  (let ((string (plist-tag-value (plump:tag-name node) :prefix)))
    (if (plusp (length string))
        (cons (plump:tag-name node) *record-parents*)
        *record-parents*)))

;;; Finalizer
;;;
(defparameter *raw-children-text-p* nil)

(defun raw-children-text-p (node)
  (plist-tag-value (plump:tag-name node) :raw-children-text-p nil))

(defun final-tag-value (name indicator &optional (stream t))
  (let ((tag-value (plist-tag-value name indicator nil))
        (tag-prefix-p (plusp (length *record-parents*)))
        (pure-tag-p (plist-tag-value name :pure-tag-p nil))
        (alternative (concatenate-keyword "alt-" indicator)))
    (when (and (plist-tag-value name alternative nil)
               (or (zerop (length *record-string*))
                   (string= *record-string* "~&")
                   (string= *record-string* "~%"))
               (setf tag-value (plist-tag-value name alternative nil))))
    (let ((control
            (case indicator
              (:open (let ((tag-prefix (if pure-tag-p
                                           +empty-string+
                                           (prefixer))))
                       (if (null tag-value)
                           +empty-string+
                           (concatenate 'string
                                        (update-spacer tag-prefix)
                                        tag-value))))
              (:close (update-spacer
                       (funcall (if tag-prefix-p 'remove-digit 'identity)
                                (or tag-value +empty-string+))))
              (otherwise +empty-string+))))
      (apply 'format stream control
             ;; Prevent not enough args
             (make-list 4 :initial-element +empty-string+)))))

(defun final-text-value (string &optional (stream t))
  (let ((before *record-string*))
    (update-spacer string)
    (when (and (plusp (length string))
               (member before (mapcar 'tail-string
                                      (indicator-values :close :style-tag-p))
                       :test 'string=))
      (unless (find (aref string 0) "?!.,:;")
        (princ +space-string+ stream))))
  (princ string stream))

;;; Repeater
;;;
(defun repeater (node indicator &optional (stream t))
  (let ((tag-value (plist-tag-value (plump:tag-name node) indicator nil)))
    (when (and (stringp tag-value)
               (>= (length tag-value) 2)
               (= 48 (char-int (character (subseq tag-value 0 1)))))
      (let* ((repeat-number (length (lquery-funcs:render-text node)))
             (control (concatenate
                       'string
                       (when (eql indicator :close) "~%")
                       (make-string repeat-number :initial-element
                                    (character (subseq tag-value 1 2)))
                       (when (eql indicator :open) "~&")
                       (subseq tag-value 2))))
        (format stream control) t))))

;;;
;;; Reference from plump:serialize-object
;;; --------------------------------------------
;;; Why lossy? (:name element-lossy-output)
;;; Because there are lost data after converted.
;;; --------------------------------------------

(defmacro define-output (function-name (plist-attribute &rest plist-tags))
  `(defun ,function-name (node &optional (stream t))
     (let ((*plist-attribute* ',plist-attribute)
           (*collection-plist-tag* ',plist-tags)
           (*record-number* 0)
           (*record-string* nil)
           (*record-parents* nil))
       (cond ((eql stream t)
              (let ((*stream* *standard-output*))
                (element-lossy-output node)))
             ((eql stream nil)
              (with-output-to-string (*stream*)
                (element-lossy-output node)))
             (t
              (let ((*stream* stream))
                (element-lossy-output node)))))))

(defmethod element-lossy-output ((node plump:text-node))
  (final-text-value (if *raw-children-text-p*
                        (plump:text node)
                        (plump:render-text node))
                    *stream*))
(defmethod element-lossy-output ((node plump:doctype)))
(defmethod element-lossy-output ((node plump:comment)))
(defmethod element-lossy-output ((node plump:element))
  (unless (repeater node :open *stream*)
    (final-tag-value (plump:tag-name node) :open *stream*))
  (when (and (not (plist-tag-value (plump:tag-name node)
                                   :swap-attributes-p nil))
             (plist-tag (plump:tag-name node)))
    (element-lossy-output (plump:attributes node)))
  (unless (skip-children-p node)
    (loop with *record-parents* = (update-prefixer node)
          and *record-number* = (update-counter node)
          and *raw-children-text-p* = (raw-children-text-p node)
          for child across (plump:children node)
          do (element-lossy-output child)))
  (unless (repeater node :close *stream*)
    (final-tag-value (plump:tag-name node) :close *stream*))
  (when (and (plist-tag-value (plump:tag-name node) :swap-attributes-p nil)
             (plist-tag (plump:tag-name node)))
    (element-lossy-output (plump:attributes node))))
(defmethod element-lossy-output ((node plump:fulltext-element)))
(defmethod element-lossy-output ((node plump:xml-header)))
(defmethod element-lossy-output ((node plump:cdata)))
(defmethod element-lossy-output ((node plump:processing-instruction)))
(defmethod element-lossy-output ((table hash-table))
  ;; Important for tags like code, img, audio, video.
  (let ((attributes (loop for key being the hash-keys of table
                          for val being the hash-values of table
                          for control = (attribute-format key)
                          unless (null control)
                            collect (format nil control val))))
    (format *stream* "~@[~{~A~}~]" attributes)))
(defmethod element-lossy-output ((node plump:nesting-node))
  (loop for child across (plump:children node)
        do (element-lossy-output child)))
(defmethod element-lossy-output ((nodes vector))
  (loop for child across nodes
        do (element-lossy-output child)))
