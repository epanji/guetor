;;;; guetor/tests.lisp

(defpackage :guetor/tests
  (:use :cl :guetor :fiveam))
(in-package :guetor/tests)

(defparameter *test-file* "test.html")
(defparameter *test-directory* (asdf:system-source-directory :guetor))
(defparameter *test-selector* "div#contents-wrapper.fancy-page.fancy-title")

(def-fixture selector-mode (mode)
  (let ((*mode* mode))
    (&body)))

(def-suite :content)
(def-suite :content-markless)
(def-suite :markless-epub)

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
  (let ((node (plump:make-element (plump:make-root) "p")))
    (is (string= (format nil "~2&") (output-markless node nil)))))

(test output-markless-for-blockquote
  (let* ((html "<blockquote><p>it's</p><cite>me</cite></blockquote>")
         (root (plump:parse html))
         (node (aref (plump:children root) 0)))
    (is (string= (format nil "| it's~&~~ me~&")
                 (output-markless node nil)))))

(test output-markless-for-cite
  (let ((node (plump:make-element (plump:make-root) "cite")))
    (is (string= (format nil "~~ ~&") (output-markless node nil)))))

(test output-markless-for-li
  (let ((node (plump:make-element (plump:make-root) "li")))
    (is (string= (format nil "- ~&") (output-markless node nil)))))

(test output-markless-for-image
  (let ((node (plump:make-element (plump:make-root) "img")))
    (is (string= (format nil "[ image  ]~&") (output-markless node nil)))))

(test output-markless-for-audio
  (let ((node (plump:make-element (plump:make-root) "audio")))
    (is (string= (format nil "[ audio  ]~&") (output-markless node nil)))))

(test output-markless-for-video
  (let ((node (plump:make-element (plump:make-root) "video")))
    (is (string= (format nil "[ video  ]~&") (output-markless node nil)))))

(test output-markless-for-strong
  (let ((node (plump:make-element (plump:make-root) "strong")))
    (is (string= (format nil " **** ") (output-markless node nil)))))

(test output-markless-for-b
  (let ((node (plump:make-element (plump:make-root) "b")))
    (is (string= (format nil " **** ") (output-markless node nil)))))

(test output-markless-for-em
  (let ((node (plump:make-element (plump:make-root) "em")))
    (is (string= (format nil " //// ") (output-markless node nil)))))

(test output-markless-for-i
  (let ((node (plump:make-element (plump:make-root) "i")))
    (is (string= (format nil " //// ") (output-markless node nil)))))

(test output-markless-for-u
  (let ((node (plump:make-element (plump:make-root) "u")))
    (is (string= (format nil " ____ ") (output-markless node nil)))))

(test output-markless-for-s
  (let ((node (plump:make-element (plump:make-root) "s")))
    (is (string= (format nil " <--> ") (output-markless node nil)))))

(test output-markless-for-code
  (let* ((html "<code data-language=\"common-lisp\"><pre>'(1)</pre></code>")
         (root (plump:parse html))
         (node (aref (plump:children root) 0)))
    (is (string= (format nil ":: common-lisp~&'(1)~&::~2&")
                 (output-markless node nil)))))

(test output-markless-for-pre
  (let ((node (plump:make-element (plump:make-root) "pre")))
    (is (string= (format nil "~&") (output-markless node nil)))))
