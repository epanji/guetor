;;;; guetor/content-markdown.lisp

(defpackage :guetor/content-markdown
  (:nicknames :guetor-c-md)
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
  (:export #:output-markdown))
(in-package :guetor/content-markdown)

(define-output output-markdown
    ((:src "(~A)~2&" :alt "[~A]" :data-language "~A~&" :href "(~A)")
     (:name "title" :open "" :close "0=~2&") ;0repeaterEOL
     (:name "h1" :open "" :close "0=~2&")    ;0repeaterEOL
     (:name "h2" :open "" :close "0-~2&")    ;0repeaterEOL
     (:name "h3" :open "### " :close "~2&")
     (:name "h4" :open "#### " :close "~2&")
     (:name "h5" :open "##### " :close "~2&")
     (:name "h6" :open "###### " :close "~2&")
     (:name "p" :open "" :close "~2&")
     (:name "cite" :open ">~&> <cite>" :close "</cite>~&" :pure-tag-p t)
     (:name "img" :open "!" :close "" :swap-attributes-p t)
     (:name "audio" :open "![]" :close "" :skip-children-p t :swap-attributes-p t)
     (:name "video" :open "![]" :close "" :skip-children-p t :swap-attributes-p t)
     (:name "strong" :open "~/guetor-o::spacer/__" :close "__" :pure-tag-p t :style-tag-p t)
     (:name "b" :open "~/guetor-o::spacer/**" :close "**" :pure-tag-p t :style-tag-p t)
     (:name "em" :open "~/guetor-o::spacer/_" :close "_" :pure-tag-p t :style-tag-p t)
     (:name "i" :open "~/guetor-o::spacer/*" :close "*" :pure-tag-p t :style-tag-p t)
     (:name "u" :open "~/guetor-o::spacer/<u>" :close "</u>" :pure-tag-p t :style-tag-p t)
     (:name "s" :open "~/guetor-o::spacer/<s>" :close "</s>" :pure-tag-p t :style-tag-p t)
     (:name "code" :open "~/guetor-o::spacer/```" :close "```" :alt-open "``` " :alt-close "```~2&" :pure-tag-p t :style-tag-p t)
     (:name "pre" :open "" :close "~&" :raw-children-text-p t)
     (:name "br" :open "~/guetor-o::spacer/" :pure-tag-p t)
     (:name "a" :open "~/guetor-o::spacer/[" :close "]" :pure-tag-p t :style-tag-p t :swap-attributes-p t)
     (:name "blockquote" :prefix "> " :absolute-prefix-p t :close "~2&")
     (:name "ul" :prefix "- " :close "~2&")
     (:name "ol" :prefix "~/guetor-o::counter/. " :close "~2&")
     (:name "table" :open "~%" :close "~&")
     (:name "thead" :open "" :close "|0---0|~&") ;side0midle0delimiterEOL
     (:name "tr" :open "| " :close "~&")
     (:name "th" :open "" :close " | ")
     (:name "td" :open "" :close " | ")))
