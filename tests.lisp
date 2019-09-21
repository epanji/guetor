;;;; guetor/tests.lisp

(defpackage :guetor/tests
  (:use :cl :guetor :fiveam)
  (:export #:suite-tests))
(in-package :guetor/tests)

(defparameter *test-file* "test.html")
(defparameter *test-directory* (asdf:system-source-directory :guetor))
(defparameter *test-selector* "div#contents-wrapper.fancy-page.fancy-title")

(def-fixture selector-mode (mode)
  (let ((*mode* mode))
    (&body)))

(def-suite :content)
(def-suite :title)
(def-suite :output)
(def-suite :content-markless)

(defun suite-tests ()
  (run! :content)
  (run! :title)
  (run! :output)
  (run! :content-markless))

(in-suite :content)

(test new-document-selector
  ;; Force to guessing for new document.
  (with-fixture selector-mode (:simple)
    (is (string= *test-selector*
                 (selector (merge-pathnames *test-file* *test-directory*)
                           t)))))

(test (new-document-last-selector :depends-on new-document-selector)
  (with-fixture selector-mode (:simple)
    (is-false (guetor::%perform-guess-p))
    (is (string= *test-selector* (selector "new input or document")))))

(test (old-document-last-selector :depends-on new-document-last-selector)
  (with-fixture selector-mode (:simple)
    (is (string= *test-selector* (selector)))))

(test (old-document-custom-selector :depends-on old-document-last-selector)
  (with-fixture selector-mode (:simple)
    (setf *selector* "div#custom")
    (is (string= "div#custom" *selector*))
    (is (string= "div#custom" (selector)))))

(test (new-document-custom-selector :depends-on old-document-custom-selector)
  (with-fixture selector-mode (:simple)
    (setf *selector* "div#custom2")
    (is (string= "div#custom2" *selector*))
    (is (string= "div#custom2" (selector "new input or document")))))

(test new-document-full-mode-selector
  (with-fixture selector-mode (:full)
    (is (eql *mode* :full))
    (is (string= (concatenate 'string "html body " *test-selector*)
                 (selector (merge-pathnames *test-file* *test-directory*)
                           t)))))

(test (new-document-default-selector :depends-on new-document-custom-selector)
  ;; Return default because invalid html for input.
  (with-fixture selector-mode (:simple)
    (is (string= "div" (selector "new input or document" t)))))

(test guess-selector-contents-wrapper
  ;; Force means perform guess selector.
  (with-fixture selector-mode (:simple)
    (is (string= *test-selector*
                 (element-selector
                  (contents-wrapper
                   (merge-pathnames *test-file* *test-directory*)
                   t))))))

(test custom-selector-contents-wrapper
  ;; Using custom selector, without guessing.
  (with-fixture selector-mode (:simple)
    (setf *selector* "div.side-content h3")
    (is (string= "div.side-content h3" (selector)))
    (is (string= "Quisque id!"
                 (plump:text
                  (contents-wrapper
                   (merge-pathnames *test-file* *test-directory*)))))))

(test only-first-guess-selector
  ;; First guess selector, then use last selector for the rest.
  (let ((file (merge-pathnames *test-file* *test-directory*)))
    (setf *selector* nil)
    (dotimes (i 3)
      (if (= 0 i)
          (is-true (nth-value 1 (contents-wrapper file)))
          (is-false (nth-value 1 (contents-wrapper file)))))))

(in-suite :title)

(test title-from-contents-wrapper
  (let ((node (contents-wrapper
               (merge-pathnames *test-file* *test-directory*)
               t)))
    (multiple-value-bind (it p) (title node)
      (is-false (null it))
      (is-true p))))

(test title-from-outside-contents
  (let* ((string (concatenate 'string
                              "<body><div class= \"something\" ><p>about</p>"
                              "</div><div><div><h1>title</h1></div></div>"
                              "<div class= \"adds\" ><p>product</p></div>"
                              "<div class= \"contents\" ><p>a</p><p>b</p>"
                              "<p>c</p></div></body>"))
         (node (plump:parse string))
         (contents (contents-wrapper node t)))
    (multiple-value-bind (it p) (title contents :text)
      (is (string= "title" it))
      (is-false p))))

(test title-as-plump-dom-element
  (let ((node (contents-wrapper
                (merge-pathnames *test-file* *test-directory*)
                t)))
    (is (typep (title node :element) 'plump-dom:element))))

(test title-as-text
  (let ((node (contents-wrapper
                (merge-pathnames *test-file* *test-directory*)
                t)))
    (is (typep (title node :text) 'string))))

(test title-as-vector
  (let ((node (contents-wrapper
                (merge-pathnames *test-file* *test-directory*)
                t)))
    (is (typep (title node :element-vector) 'vector))
    (is (typep (title node :text-vector) 'vector))))

(in-suite :output)

(test exported-symbols
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "*COLLECTION-PLIST-TAG*"
                         :guetor/output))))
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "*PLIST-ATTRIBUTE*"
                         :guetor/output))))
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "*RECORD-PARENTS*"
                         :guetor/output))))
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "*RECORD-NUMBER*"
                         :guetor/output))))
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "*RECORD-STRING*"
                         :guetor/output))))
  (is (eql :EXTERNAL
           (nth-value 1 (find-symbol
                         "DEFINE-OUTPUT"
                         :guetor/output)))))

(in-suite :content-markless)

(test contents-wrapper-to-markless-string
  (let ((node (contents-wrapper
               (merge-pathnames *test-file* *test-directory*)
               t)))
    ;; Only check for header 1 on this test.
    (is (string= "# " (subseq (output-markless node nil) 0 2)))))

(test output-markless-for-first-header
  (let ((node (plump:make-element (plump:make-root) "h1")))
    (is (string= (format nil "# ~2&") (output-markless node nil)))))

(test output-markless-for-second-header
  (let ((node (plump:make-element (plump:make-root) "h2")))
    (is (string= (format nil "## ~2&") (output-markless node nil)))))

(test output-markless-for-third-header
  (let ((node (plump:make-element (plump:make-root) "h3")))
    (is (string= (format nil "### ~2&") (output-markless node nil)))))

(test output-markless-for-forth-header
  (let ((node (plump:make-element (plump:make-root) "h4")))
    (is (string= (format nil "#### ~2&") (output-markless node nil)))))

(test output-markless-for-fifth-header
  (let ((node (plump:make-element (plump:make-root) "h5")))
    (is (string= (format nil "##### ~%~2&") (output-markless node nil)))))

(test output-markless-for-paragraph
  (let ((node (plump:parse "<p>paragraph</p>")))
    (is (string= (format nil "paragraph~2&") (output-markless node nil)))))

(test output-markless-for-blockquote
  (let* ((html "<blockquote><p>it's</p><cite>me</cite></blockquote>")
         (node (plump:parse html)))
    (is (string= (format nil "| it's~&~~ me~2&")
                 (output-markless node nil)))))

(test output-markless-for-cite
  (let ((node (plump:parse "<cite>name</cite>")))
    (is (string= (format nil "~~ name~&") (output-markless node nil)))))

(test output-markless-for-li
  ;; Nested
  (let ((node-ul (plump:parse "<ul><li><p>a</p></li><li><p>b</p></li></ul>"))
        (node-ol (plump:parse "<ol><li><p>a</p></li><li><p>b</p></li></ol>"))
        (node-nested (plump:parse "<ol><li><p>a</p></li><li><p>b</p><ul>
<li><p>ba</p></li><li><p>bb</p></li></ul></li></ol>")))
    (is (string= (format nil "- a~&- b~2&") (output-markless node-ul nil)))
    (is (string= (format nil "1. a~&2. b~2&")
                 (output-markless node-ol nil)))
    (is (string= (format nil "1. a~&2. b~&   - ba~&   - bb~2&")
                 (output-markless node-nested nil)))))

(test output-markless-for-image
  (let ((node (plump:parse "<img src=\"file.png\" />")))
    (is (string= (format nil "[ image file.png ]~2&")
                 (output-markless node nil)))))

(test output-markless-for-audio
  (let ((node (plump:parse "<audio src=\"file.mp3\">skip</audio>")))
    (is (string= (format nil "[ audio file.mp3 ]~2&")
                 (output-markless node nil)))))

(test output-markless-for-video
  (let ((node (plump:parse "<video src=\"file.mp4\">skip</video>")))
    (is (string= (format nil "[ video file.mp4 ]~2&")
                 (output-markless node nil)))))

(test output-markless-for-strong
  (let ((node (plump:parse "<strong>bold</strong>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <strong>text</strong> is <strong>bold</strong>!</p>")))
    (is (string= (format nil "**bold**") (output-markless node nil)))
    (is (string= (format nil "the **text** is **bold**!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-b
  (let ((node (plump:parse "<b>bold</b>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <b>text</b> is <b>bold</b>!</p>")))
    (is (string= (format nil "**bold**") (output-markless node nil)))
    (is (string= (format nil "the **text** is **bold**!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-em
  (let ((node (plump:parse "<em>italic</em>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <em>text</em> is <em>italic</em>!</p>")))
    (is (string= (format nil "//italic//") (output-markless node nil)))
    (is (string= (format nil "the //text// is //italic//!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-i
  (let ((node (plump:parse "<i>italic</i>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <i>text</i> is <i>italic</i>!</p>")))
    (is (string= (format nil "//italic//") (output-markless node nil)))
    (is (string= (format nil "the //text// is //italic//!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-u
  (let ((node (plump:parse "<u>underline</u>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <u>text</u> is <u>underline</u>!</p>")))
    (is (string= (format nil "__underline__") (output-markless node nil)))
    (is (string= (format nil "the __text__ is __underline__!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-s
  (let ((node (plump:parse "<s>strikethrough</s>"))
        (node-for-spacer-test
          (plump:parse
           "<p>the <s>text</s> is <s>strikethrough</s>!</p>")))
    (is (string= (format nil "<-strikethrough->") (output-markless node nil)))
    (is (string= (format nil "the <-text-> is <-strikethrough->!~2&")
                 (output-markless node-for-spacer-test nil)))))

(test output-markless-for-code
  (let ((node (plump:parse
               (concatenate 'string
                            "<code data-language=\"common-lisp\">"
                            "<pre>(+ 1 2)"
                            (string #\Newline)
                            "(+ 1 2 3)"
                            "</pre>"
                            "</code>")))
        (node-inline
          (plump:parse "<p>Nibh <code>tortor</code> id aliquet!</p>")))
    (is (string= (format nil ":: common-lisp~&(+ 1 2)~&(+ 1 2 3)~&::~2&")
                 (output-markless node nil)))
    (is (string= (format nil "Nibh ``tortor`` id aliquet!~2&")
                 (output-markless node-inline nil)))))

(test output-markless-for-pre
  (let ((node (plump:parse "<pre>(+ 1 2)</pre>")))
    (is (string= (format nil "(+ 1 2)~&") (output-markless node nil)))))

(test output-markless-for-br
  ;; Tag br treated as spacer.
  (let ((node (plump:parse "<p>one<br>two three</p>")))
    (is (string= (format nil "one two three~2&")
                 (output-markless node nil)))))

(test output-markless-for-a
  (let ((node (plump:parse "<p>the <a href=\"#id\">link</a> text</p")))
    (is (string= (format nil "the \"link\"(#id) text~2&")
                 (output-markless node nil)))))
