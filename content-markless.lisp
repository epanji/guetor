;;;; guetor/content-markless.lisp

(defpackage :guetor/content-markless
  (:use :cl)
  (:import-from :plump #:*stream*)
  (:import-from :guetor/output
                #:*collection-plist-tag*
                #:*plist-attribute*
                ;;----------------------
                #:*record-parents*
                #:*record-number*
                #:*record-string*
                ;;----------------------
                #:define-output)
  (:export #:output-markless))
(in-package :guetor/content-markless)

(define-output output-markless
    ((:src "~A" :data-language "~A~&")
     (:name "title" :open "# " :close "~2&")
     (:name "h1" :open "# " :close "~2&")
     (:name "h2" :open "## " :close "~2&")
     (:name "h3" :open "### " :close "~2&")
     (:name "h4" :open "#### " :close "~2&")
     (:name "h5" :open "##### " :close "~2&")
     (:name "p" :open "" :close "~2&")
     (:name "cite" :open "~~ " :close "~&" :pure-tag-p t)
     (:name "img" :open "[ image " :close " ]~&")
     (:name "audio" :open "[ audio " :close " ]~&" :skip-children-p t)
     (:name "video" :open "[ video " :close " ]~&" :skip-children-p t)
     (:name "strong" :open "~/guetor-o::spacer/**" :close "**":pure-tag-p t :style-tag-p t)
     (:name "b" :open "~/guetor-o::spacer/**" :close "**" :pure-tag-p t :style-tag-p t)
     (:name "em" :open "~/guetor-o::spacer///" :close "//" :pure-tag-p t :style-tag-p t)
     (:name "i" :open "~/guetor-o::spacer///" :close "//" :pure-tag-p t :style-tag-p t)
     (:name "u" :open "~/guetor-o::spacer/__" :close "__" :pure-tag-p t :style-tag-p t)
     (:name "s" :open "~/guetor-o::spacer/<-" :close "->" :pure-tag-p t :style-tag-p t)
     (:name "code" :open "~/guetor-o::spacer/``" :close "``" :alt-open ":: " :alt-close "::~2&" :pure-tag-p t :style-tag-p t)
     (:name "pre" :open "" :close "~&" :raw-children-text-p t)
     (:name "br" :open "~/guetor-o::spacer/" :pure-tag-p t)
     (:name "a" :open "~/guetor-o::spacer/__" :close "__" :pure-tag-p t)
     (:name "blockquote" :prefix "| " :absolute-prefix-p t :close "~2&")
     (:name "ul" :prefix "- " :close "~2&")
     (:name "ol" :prefix "~/guetor-o::counter/. " :close "~2&")))
