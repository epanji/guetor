;;;; guetor/content-markless.lisp

(defpackage :guetor/content-markless
  (:nicknames :guetor-c-m)
  (:use :cl)
  (:import-from :plump *stream*)
  (:export #:output-markless))
(in-package :guetor/content-markless)

;;; Tag open and close rules:
;;; - Forbid space or end of line on first character opening tag.
;;; - May have end of line but not space on last character closing tag.
;;; - Tag that affect multiple tags have to set line prefix.
;;; - Line prefix can not affect tag with pure-p value T.
(defparameter *collection-plist-tag*
  '((:name "h1" :open "# " :close "~2&")
    (:name "h2" :open "## " :close "~2&")
    (:name "h3" :open "### " :close "~2&")
    (:name "h4" :open "#### " :close "~2&")
    (:name "h5" :open "##### " :close "~2&")
    (:name "p" :open "" :close "~2&")
    (:name "cite" :open "~~ " :close "~&" :pure-p t)
    (:name "img" :open "[ image " :close " ]~&")
    (:name "audio" :open "[ audio " :close " ]~&")
    (:name "video" :open "[ video " :close " ]~&")
    (:name "strong" :open "~/guetor-c-m::spacer/**" :close "**":pure-p t)
    (:name "b" :open "~/guetor-c-m::spacer/**" :close "**" :pure-p t)
    (:name "em" :open "~/guetor-c-m::spacer///" :close "//" :pure-p t)
    (:name "i" :open "~/guetor-c-m::spacer///" :close "//" :pure-p t)
    (:name "u" :open "~/guetor-c-m::spacer/__" :close "__" :pure-p t)
    (:name "s" :open "~/guetor-c-m::spacer/<-" :close "->" :pure-p t)
    (:name "code" :open ":: " :close "::~2&")
    (:name "pre" :open "" :close "~&")
    (:name "br" :open "~/guetor-c-m::spacer/" :pure-p t)
    (:name "a" :open "~/guetor-c-m::spacer/__" :close "__" :pure-p t)
    ;; Prefixed
    (:name "blockquote" :prefix "| " :close "~2&")
    (:name "ul" :prefix "- " :close "~2&")
    (:name "ol" :prefix "~/guetor-c-m::counter/. " :close "~2&")))

(defparameter *plist-attributes*
  '(:src "~A" :data-language "~A~&"))



(defvar *tag-prefix* "")
(defvar *counter* 0)
(defvar *spacer* nil)

(defun counter (stream int &optional colon at-sign)
  (declare (ignore colon at-sign))
  (princ int stream))

(defun spacer (stream arg &optional colon at-sign)
  (declare (ignore arg colon at-sign))
  (princ (if (or (null *spacer*)
                 (zerop (length *spacer*))
                 (string= *spacer* "~&")
                 (string= *spacer* "~%"))
             ;; Empty or Space
             "" " ")
         stream))



(defun remove-digit (string)
  (remove-if 'digit-char-p string))

(defun indicator-value (plist &optional (indicator :name) (default ""))
  (getf plist indicator default))

(defun plist-tag (name)
  (find name *collection-plist-tag* :test 'string= :key 'indicator-value))

(defun plist-tag-value (name indicator &optional (default ""))
  (let ((plist-tag (plist-tag name)))
    (indicator-value plist-tag indicator default)))

(defun final-tag-value (name indicator &optional (stream t))
  (let ((tag-value (plist-tag-value name indicator nil))
        (tag-prefix-p (plusp (length *tag-prefix*)))
        (pure-p (plist-tag-value name :pure-p nil)))
    (format stream (case indicator
                     (:open (if (null tag-value) ""
                                (concatenate 'string
                                             (if pure-p
                                                 ""
                                                 *tag-prefix*)
                                             tag-value)))
                     (:close (update-spacer
                              (funcall (if tag-prefix-p
                                           'remove-digit
                                           'identity)
                                       (or tag-value ""))))
                     (otherwise ""))
            *counter*)))

(defun final-text-value (string &optional (stream t))
  (let ((before *spacer*))
    (update-spacer string)
    ;; Space after tag decorator.
    (when (and (plusp (length string))
               (or (string= before "**")
                   (string= before "//")
                   (string= before "__")
                   (string= before "->")))
      (unless (find (aref string 0) "?!.,:;")
        (princ " " stream))))
  (plump:encode-entities string stream))

(defun attribute-format (name)
  (indicator-value *plist-attributes*
                   (intern (string-upcase name) :keyword)
                   nil))



(defun concatenate-tag-prefix (node)
  (let ((string (plist-tag-value (plump:tag-name node) :prefix)))
    (if (and (plusp (length string))
             (string/= *tag-prefix* string))
        (concatenate 'string *tag-prefix* string)
        *tag-prefix*)))

(defun update-counter (node)
  (cond ((string= (plump:tag-name node) "ol") 0)
        ((string= (plump:tag-name node) "li") (incf *counter*))
        (t *counter*)))

(defun update-spacer (string)
  (let* ((no-digit (remove-digit string))
         (length (length no-digit)))
    (when (plusp length)
      (setf *spacer* (subseq no-digit (max 0 (- length 2))))))
  string)

(defun traverse-children-p (node)
  (and (plusp (length (plump:children node)))
       (string/= (plump:tag-name node) "audio")
       (string/= (plump:tag-name node) "video")
       (string/= (plump:tag-name node) "title")))

;;;
;;; Reference from plump:serialize-object
;;; --------------------------------------------
;;; Why lossy? (:name element-lossy-markless)
;;; Because there are lost data after converted.
;;; --------------------------------------------

(defun output-markless (node &optional (stream t))
  (setf *spacer* "~&")
  (cond ((eql stream t)
         (let ((*stream* *standard-output*))
           (element-lossy-markless node)))
        ((eql stream nil)
         (with-output-to-string (*stream*)
           (element-lossy-markless node)))
        (t
         (let ((*stream* stream))
           (element-lossy-markless node)))))

(defmethod element-lossy-markless ((node plump:text-node))
  (final-text-value (plump:render-text node) *stream*))
(defmethod element-lossy-markless ((node plump:doctype)))
(defmethod element-lossy-markless ((node plump:comment)))
(defmethod element-lossy-markless ((node plump:element))
  (final-tag-value (plump:tag-name node) :open *stream*)
  (element-lossy-markless (plump:attributes node))
  (when (traverse-children-p node)
    (loop with *tag-prefix* = (concatenate-tag-prefix node)
          and *counter* = (update-counter node)
          for child across (plump:children node)
          do (element-lossy-markless child)))
  (final-tag-value (plump:tag-name node) :close *stream*))
(defmethod element-lossy-markless ((node plump:fulltext-element)))
(defmethod element-lossy-markless ((node plump:xml-header)))
(defmethod element-lossy-markless ((node plump:cdata)))
(defmethod element-lossy-markless ((node plump:processing-instruction)))
(defmethod element-lossy-markless ((table hash-table))
  ;; Important for tags like code, img, audio, video.
  (let ((attributes (loop for key being the hash-keys of table
                          for val being the hash-values of table
                          for control = (attribute-format key)
                          unless (null control)
                            collect (format nil control val))))
    (format *stream* "~@[~{~a~^ ~}~]" attributes)))
(defmethod element-lossy-markless ((node plump:nesting-node))
  (loop for child across (plump:children node)
        do (element-lossy-markless child)))
(defmethod element-lossy-markless ((nodes vector))
  (loop for child across nodes
        do (element-lossy-markless child)))
