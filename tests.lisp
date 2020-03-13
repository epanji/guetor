;;;; guetor/tests.lisp

(defpackage :guetor/tests
  (:use :cl :guetor :fiveam)
  (:export #:suite-tests))
(in-package :guetor/tests)

(defparameter *test-file* "test.html")
(defparameter *test-directory* (asdf:system-source-directory :guetor))
(defparameter *test-selector* "div#contents-wrapper.fancy-page.fancy-title")

(defparameter *html-sample-map*
  '(("h1" . "<h1>header 1</h1>")
    ("h2" . "<h2>header 2</h2>")
    ("h3" . "<h3>header 3</h3>")
    ("h4" . "<h4>header 4</h4>")
    ("h5" . "<h5>header 5</h5>")
    ("h6" . "<h6>header 6</h6>")
    ("p" . "<p>paragraph</p>")
    ("blockquote" . "<blockquote><p>it's</p><cite>me</cite></blockquote>")
    ("cite" . "<cite>name</cite>")
    ("ul" . "<ul><li><p>a</p></li><li><p>b</p></li></ul>")
    ("ol" . "<ol><li><p>a</p></li><li><p>b</p></li></ol>")
    ("nested" . "<ol>
                   <li><p>a</p></li>
                   <li><p>b</p>
                     <ul>
                       <li><p>ba</p></li>
                       <li><p>bb</p></li>
                     </ul>
                   </li>
                 </ol>")
    ("image" . "<img src=\"file.png\" />")
    ("audio" . "<audio src=\"file.mp3\">skip</audio>")
    ("video" . "<video src=\"file.mp4\">skip</video>")
    ("strong" . "<strong>bold</strong>")
    ("strong!" . "<p>the <strong>text</strong> is <strong>bold</strong>!</p>")
    ("b" . "<b>bold</b>")
    ("b!" . "<p>the <b>text</b> is <b>bold</b>!</p>")
    ("em" . "<em>italic</em>")
    ("em!" . "<p>the <em>text</em> is <em>italic</em>!</p>")
    ("i" . "<i>italic</i>")
    ("i!" . "<p>the <i>text</i> is <i>italic</i>!</p>")
    ("u" . "<u>underline</u>")
    ("u!" . "<p>the <u>text</u> is <u>underline</u>!</p>")
    ("s" . "<s>strikethrough</s>")
    ("s!" . "<p>the <s>text</s> is <s>strikethrough</s>!</p>")
    ("code" . #.(format nil "<code data-language=\"common-lisp\">~
                               <pre>~
                                 (+ 1 2)~&~
                                 (+ 1 2 3)~
                               </pre>~
                             </code>"))
    ("code-in" . "<p>Nibh <code>tortor</code> id aliquet!</p>")
    ("pre" . "<pre>(+ 1 2)</pre>")
    ("br" . "<p>one<br>two three</p>")
    ("a" . "<p>the <a href=\"#id\">link</a> text</p")))

(defun sample-from-html (sample-name)
  (declare (string sample-name))
  (let ((pair (assoc sample-name *html-sample-map* :test 'string-equal)))
    (if (null pair)
        (error "No ~S inside `*html-sample-map*'." sample-name)
        (plump:parse (cdr pair)))))

(defun output-from-sample (sample-name fun)
  (declare (string sample-name) (function fun))
  (funcall fun (sample-from-html sample-name) nil))

(def-fixture selector-mode (mode)
  (let ((*mode* mode))
    (&body)))

(def-suite :condition)
(def-suite :content)
(def-suite :title)
(def-suite :output)
(def-suite :navigation)
(def-suite :content-markless)
(def-suite :content-markdown)

(defun suite-tests ()
  (run! :condition)
  (run! :content)
  (run! :title)
  (run! :output)
  (run! :navigation)
  (run! :content-markless)
  (run! :content-markdown))

(in-suite :condition)

(test inherited-symbols
  (is (eql :INHERITED
           (nth-value 1 (find-symbol
                         "GUETOR-CONDITION"
                         :guetor))))
  (is (eql :INHERITED
           (nth-value 1 (find-symbol
                         "GUETOR-ERROR"
                         :guetor))))
  (is (eql :INHERITED
           (nth-value 1 (find-symbol
                         "GUETOR-WARNING"
                         :guetor)))))

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

(in-suite :navigation)

(test find-navigation-forward
  (let ((path (merge-pathnames *test-file* *test-directory*))
        (*navigation-base* "https://sample.org"))
    (is (concatenate 'string "/next" *navigation-base*)
        (find-navigation (guetor:document path)))))

(test find-navigation-backward
  (let ((path (merge-pathnames *test-file* *test-directory*))
        (*navigation-base* "https://sample.org"))
    (is (concatenate 'string "/prev" *navigation-base*)
        (find-navigation (guetor:document path) :backward))))

(in-suite :content-markless)

(test output-markless-for-first-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h1" fun)
                 (format nil "# header 1~2&")))))

(test output-markless-for-second-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h2" fun)
                 (format nil "## header 2~2&")))))

(test output-markless-for-third-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h3" fun)
                 (format nil "### header 3~2&")))))

(test output-markless-for-forth-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h4" fun)
                 (format nil "#### header 4~2&")))))

(test output-markless-for-fifth-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h5" fun)
                 (format nil "##### header 5~2&")))))

(test output-markless-for-sixth-header
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "h6" fun)
                 (format nil "###### header 6~2&")))))

(test output-markless-for-paragraph
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "p" fun)
                 (format nil "paragraph~2&")))))

(test output-markless-for-blockquote
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "blockquote" fun)
                 (format nil "| it's~&~~ me~2&")))))

(test output-markless-for-cite
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "cite" fun)
                 (format nil "~~ name~&")))))

(test output-markless-for-li
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "ul" fun)
                 (format nil "- a~&- b~2&")))
    (is (string= (output-from-sample "ol" fun)
                 (format nil "1. a~&2. b~2&")))
    (is (string= (output-from-sample "nested" fun)
                 (format nil "1. a~&2. b~&   - ba~&   - bb~2&")))))

(test output-markless-for-image
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "image" fun)
                 (format nil "[ image file.png ]~2&")))))

(test output-markless-for-audio
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "audio" fun)
                 (format nil "[ audio file.mp3 ]~2&")))))

(test output-markless-for-video
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "video" fun)
                 (format nil "[ video file.mp4 ]~2&")))))

(test output-markless-for-strong
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "strong" fun)
                 (format nil "**bold**")))
    (is (string= (output-from-sample "strong!" fun)
                 (format nil "the **text** is **bold**!~2&")))))

(test output-markless-for-b
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "b" fun)
                 (format nil "**bold**")))
    (is (string= (output-from-sample "b!" fun)
                 (format nil "the **text** is **bold**!~2&")))))

(test output-markless-for-em
    (let ((fun (function output-markless)))
      (is (string= (output-from-sample "em" fun)
                   (format nil "//italic//")))
      (is (string= (output-from-sample "em!" fun)
                   (format nil "the //text// is //italic//!~2&")))))

(test output-markless-for-i
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "i" fun)
                 (format nil "//italic//")))
    (is (string= (output-from-sample "i!" fun)
                 (format nil "the //text// is //italic//!~2&")))))

(test output-markless-for-u
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "u" fun)
                 (format nil "__underline__")))
    (is (string= (output-from-sample "u!" fun)
                 (format nil "the __text__ is __underline__!~2&")))))

(test output-markless-for-s
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "s" fun)
                 (format nil "<-strikethrough->")))
    (is (string= (output-from-sample "s!" fun)
                 (format nil "the <-text-> is <-strikethrough->!~2&")))))

(test output-markless-for-code
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "code" fun)
                 (format nil ":: common-lisp~&(+ 1 2)~&(+ 1 2 3)~&::~2&")))
    (is (string= (output-from-sample "code-in" fun)
                 (format nil "Nibh ``tortor`` id aliquet!~2&")))))

(test output-markless-for-pre
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "pre" fun)
                 (format nil "(+ 1 2)~&")))))

(test output-markless-for-br
  ;; Tag br treated as spacer.
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "br" fun)
                 (format nil "one two three~2&")))))

(test output-markless-for-a
  (let ((fun (function output-markless)))
    (is (string= (output-from-sample "a" fun)
                 (format nil "the \"link\"(#id) text~2&")))))

(in-suite :content-markdown)

(test output-markdown-for-first-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h1" fun)
                 (format nil "header 1~&========~2&")))))

(test output-markdown-for-second-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h2" fun)
                 (format nil "header 2~&--------~2&")))))

(test output-markdown-for-third-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h3" fun)
                 (format nil "### header 3~2&")))))

(test output-markdown-for-forth-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h4" fun)
                 (format nil "#### header 4~2&")))))

(test output-markdown-for-fifth-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h5" fun)
                 (format nil "##### header 5~2&")))))

(test output-markdown-for-sixth-header
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "h6" fun)
                 (format nil "###### header 6~2&")))))

(test output-markdown-for-paragraph
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "p" fun)
                 (format nil "paragraph~2&")))))

(test output-markdown-for-blockquote
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "blockquote" fun)
                 (format nil "> it's~&>~&> <cite>me</cite>~2&")))))

(test output-markdown-for-cite
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "cite" fun)
                 (format nil ">~&> <cite>name</cite>~&")))))

(test output-markdown-for-li
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "ul" fun)
                 (format nil "- a~&- b~2&")))
    (is (string= (output-from-sample "ol" fun)
                 (format nil "1. a~&2. b~2&")))
    (is (string= (output-from-sample "nested" fun)
                 (format nil "1. a~&2. b~&   - ba~&   - bb~2&")))))

(test output-markdown-for-image
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "image" fun)
                 (format nil "!(file.png)~2&")))))

(test output-markdown-for-audio
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "audio" fun)
                 (format nil "![](file.mp3)~2&")))))

(test output-markdown-for-video
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "video" fun)
                 (format nil "![](file.mp4)~2&")))))

(test output-markdown-for-strong
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "strong" fun)
                 (format nil "__bold__")))
    (is (string= (output-from-sample "strong!" fun)
                 (format nil "the __text__ is __bold__!~2&")))))

(test output-markdown-for-b
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "b" fun)
                 (format nil "**bold**")))
    (is (string= (output-from-sample "b!" fun)
                 (format nil "the **text** is **bold**!~2&")))))

(test output-markdown-for-em
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "em" fun)
                 (format nil "_italic_")))
    (is (string= (output-from-sample "em!" fun)
                 (format nil "the _text_ is _italic_!~2&")))))

(test output-markdown-for-i
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "i" fun)
                 (format nil "*italic*")))
    (is (string= (output-from-sample "i!" fun)
                 (format nil "the *text* is *italic*!~2&")))))

(test output-markdown-for-u
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "u" fun)
                 (format nil "<u>underline</u>")))
    (is (string= (output-from-sample "u!" fun)
                 (format nil "the <u>text</u> is <u>underline</u>!~2&")))))

(test output-markdown-for-s
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "s" fun)
                 (format nil "<s>strikethrough</s>")))
    (is (string= (output-from-sample "s!" fun)
                 (format nil
                         "the <s>text</s> is <s>strikethrough</s>!~2&")))))

(test output-markdown-for-code
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "code" fun)
                 (format nil "``` common-lisp~&(+ 1 2)~&(+ 1 2 3)~&```~2&")))
    (is (string= (output-from-sample "code-in" fun)
                 (format nil "Nibh ```tortor``` id aliquet!~2&")))))

(test output-markdown-for-pre
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "pre" fun)
                 (format nil "(+ 1 2)~&")))))

(test output-markdown-for-br
  ;; Tag br treated as spacer.
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "br" fun)
                 (format nil "one two three~2&")))))

(test output-markdown-for-a
  (let ((fun (function output-markdown)))
    (is (string= (output-from-sample "a" fun)
                 (format nil "the [link](#id) text~2&")))))
