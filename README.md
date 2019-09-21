GUETOR: Guess Selector
======================

GUETOR is system to guess selector for contents wrapper in HTML.
It provide utility to automatically guess or manually set CSS selector for contents wrapper.
It will retrieve Plump DOM as result that could be converted to desired format.

The files that constitute GUETOR are:

* [Interface](interface.lisp)
  provide internal and external symbol for GUETOR system.

* [Content](content.lisp)
  provide automatic selector, custom selector and contents wrapper retriever.

* [Title](title.lisp)
  provide title retriever for contents wrapper.

* [Output](output.lisp)
  provide system to be inherited by others for lossy data convertion.

* [Content-markless](content-markless.lisp)
  provide function to convert Plump DOM to markless format.

* [Content-markdown](content-markdown.lisp)
  provide function to convert Plump DOM to markdown format.

Related System:
----------------

* [lQuery](https://shinmera.github.io/lquery)
* [Plump](https://shinmera.github.io/plump)
* [CLSS](https://shinmera.github.io/CLSS)

How to use?
-----------

Load system by quicklisp.

``` common-lisp
(ql:quickload "guetor")
```

Load system by ASDF.

``` common-lisp
(asdf:load-system "guetor")
```

Get CSS selector with options SIMPLE and FULL mode, default is SIMPLE.

``` common-lisp
(guetor:selector #p"test.html" t)
=> "div#contents-wrapper.fancy-page.fancy-title"

(let ((guetor:*mode* :full)) (guetor:selector #p"test.html" t))
=> "html body div#contents-wrapper.fancy-page.fancy-title"
```

Get contents wrapper with two values as results. First is Plump DOM and second is predicate for performing guess.

``` common-lisp
(guetor:contents-wrapper #p"test.html" t)
=> #<PLUMP-DOM:ELEMENT div {10046FF773}>
=> T
```

Get title with two values as results. First is the title and second is predicate for contents having title. In case no title inside contents, it will searching up until top document.

``` common-lisp
(guetor:title (guetor:contents-wrapper #p"test.html" t))
=> #<PLUMP-DOM:ELEMENT h1 {1001FAF783}>
=> T
(guetor:title (guetor:contents-wrapper #p"test.html" t) :text)
=> "Arcu, non sodales!"
=> T
```

How to convert?
---------------

Convert to html by Plump.

``` common-lisp
(plump:serialize (guetor:contents-wrapper #p"test.html" t) nil)
=> "<div id=\"contents-wrapper\" class=\"fancy-page fancy-title\">
...
...
...
        </div>"
```

Convert to markless by GUETOR with full text sample for review.

``` common-lisp
(guetor:output-markless (guetor:contents-wrapper #p"test.html" t) nil)
=> "# Arcu, non sodales!

Quam pellentesque nec nam aliquam sem et tortor consequat id porta nibh venenatis cras sed felis eget velit aliquet? Vulputate enim nulla aliquet porttitor lacus, luctus accumsan tortor posuere ac?

Cursus euismod quis viverra nibh cras pulvinar mattis nunc, sed blandit libero volutpat sed cras ornare arcu dui vivamus arcu felis, bibendum ut tristique.

Ultricies leo integer malesuada nunc vel risus commodo viverra maecenas accumsan, lacus vel facilisis volutpat, est velit egestas dui, id ornare arcu.

:: common-lisp
(+ 1 2)
(+ 1 2 3)
(+ 1 2 3 4)
::

**Posuere ac:**

1. **Felis** eget nunc.
2. Ac felis ``donec``.
3. Diam ``quis`` enim.
   - Tristique sollicitudin nibh.
   - Diam sollicitudin tempor.

| Neque aliquam **vestibulum** morbi blandit cursus risus, at ultrices mi tempus imperdiet nulla malesuada pellentesque //elit//. Purus sit amet luctus venenatis, __lectus__ magna fringilla urna, porttitor rhoncus dolor <-purus-> non.
| - Amet nulla facilisi!
| - Eget dolor morbi.
| - Pretium quam vulputate.
~ Lorem

"
```

Author
------

Panji Kusuma (epanji@gmail.com)

_Notes:_

_Always remember when converting to desired format, it will not be able to convert back with the same result as before. (lossy)_

Tests
-----

``` common-lisp
CL-USER> (asdf:test-system "guetor")

Running test suite CONTENT
 Running test NEW-DOCUMENT-SELECTOR .
 Running test NEW-DOCUMENT-LAST-SELECTOR ..
 Running test OLD-DOCUMENT-LAST-SELECTOR .
 Running test OLD-DOCUMENT-CUSTOM-SELECTOR ..
 Running test NEW-DOCUMENT-CUSTOM-SELECTOR ..
 Running test NEW-DOCUMENT-FULL-MODE-SELECTOR ..
 Running test NEW-DOCUMENT-DEFAULT-SELECTOR .
 Running test GUESS-SELECTOR-CONTENTS-WRAPPER .
 Running test CUSTOM-SELECTOR-CONTENTS-WRAPPER ..
 Running test ONLY-FIRST-GUESS-SELECTOR ...
 Did 17 checks.
    Pass: 17 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite TITLE
 Running test TITLE-FROM-CONTENTS-WRAPPER ..
 Running test TITLE-FROM-OUTSIDE-CONTENTS ..
 Running test TITLE-AS-PLUMP-DOM-ELEMENT .
 Running test TITLE-AS-TEXT .
 Running test TITLE-AS-VECTOR ..
 Did 8 checks.
    Pass: 8 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite OUTPUT
 Running test EXPORTED-SYMBOLS ......
 Did 6 checks.
    Pass: 6 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite CONTENT-MARKLESS
 Running test CONTENTS-WRAPPER-TO-MARKLESS-STRING .
 Running test OUTPUT-MARKLESS-FOR-FIRST-HEADER .
 Running test OUTPUT-MARKLESS-FOR-SECOND-HEADER .
 Running test OUTPUT-MARKLESS-FOR-THIRD-HEADER .
 Running test OUTPUT-MARKLESS-FOR-FORTH-HEADER .
 Running test OUTPUT-MARKLESS-FOR-FIFTH-HEADER .
 Running test OUTPUT-MARKLESS-FOR-PARAGRAPH .
 Running test OUTPUT-MARKLESS-FOR-BLOCKQUOTE .
 Running test OUTPUT-MARKLESS-FOR-CITE .
 Running test OUTPUT-MARKLESS-FOR-LI ...
 Running test OUTPUT-MARKLESS-FOR-IMAGE .
 Running test OUTPUT-MARKLESS-FOR-AUDIO .
 Running test OUTPUT-MARKLESS-FOR-VIDEO .
 Running test OUTPUT-MARKLESS-FOR-STRONG ..
 Running test OUTPUT-MARKLESS-FOR-B ..
 Running test OUTPUT-MARKLESS-FOR-EM ..
 Running test OUTPUT-MARKLESS-FOR-I ..
 Running test OUTPUT-MARKLESS-FOR-U ..
 Running test OUTPUT-MARKLESS-FOR-S ..
 Running test OUTPUT-MARKLESS-FOR-CODE ..
 Running test OUTPUT-MARKLESS-FOR-PRE .
 Running test OUTPUT-MARKLESS-FOR-BR .
 Running test OUTPUT-MARKLESS-FOR-A .
 Did 32 checks.
    Pass: 32 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```