# GUETOR

### _Panji Kusuma <epanji@gmail.com>_

Guetor is system to guess selector for contents wrapper.

## Usage

``` common-lisp
(ql:quickload 'guetor)

(guetor:selector #p"test.html" t)
=> "div#contents-wrapper.fancy-page.fancy-title"

(let ((guetor:*mode* :full)) (guetor:selector #p"test.html" t))
=> "html body div#contents-wrapper.fancy-page.fancy-title"
```

``` common-lisp
CL-USER> (guetor:output-markless (guetor:contents-wrapper #p"test.html" t) nil)
"# Arcu, non sodales!

Quam pellentesque nec nam aliquam sem et tortor consequat id porta nibh venenatis cras sed felis eget velit aliquet? Vulputate enim nulla aliquet porttitor lacus, luctus accumsan tortor posuere ac?

Cursus euismod quis viverra nibh cras pulvinar mattis nunc, sed blandit libero volutpat sed cras ornare arcu dui vivamus arcu felis, bibendum ut tristique.

Ultricies leo integer malesuada nunc vel risus commodo viverra maecenas accumsan, lacus vel facilisis volutpat, est velit egestas dui, id ornare arcu.

:: common-lisp
(+ 1 2)
(+ 1 2 3)
(+ 1 2 3 4)
::

| Neque aliquam **vestibulum** morbi blandit cursus risus, at ultrices mi tempus imperdiet nulla malesuada pellentesque //elit// . Purus sit amet luctus venenatis, __lectus__ magna fringilla urna, porttitor rhoncus dolor <-purus-> non.
| - Amet nulla facilisi!
| - Eget dolor morbi.
| - Pretium quam vulputate.
~ Lorem
"
```

## Tests

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
 Running test OUTPUT-MARKLESS-FOR-LI .
 Running test OUTPUT-MARKLESS-FOR-IMAGE .
 Running test OUTPUT-MARKLESS-FOR-AUDIO .
 Running test OUTPUT-MARKLESS-FOR-VIDEO .
 Running test OUTPUT-MARKLESS-FOR-STRONG .
 Running test OUTPUT-MARKLESS-FOR-B .
 Running test OUTPUT-MARKLESS-FOR-EM .
 Running test OUTPUT-MARKLESS-FOR-I .
 Running test OUTPUT-MARKLESS-FOR-U .
 Running test OUTPUT-MARKLESS-FOR-S .
 Running test OUTPUT-MARKLESS-FOR-CODE .
 Running test OUTPUT-MARKLESS-FOR-PRE .
 Did 38 checks.
    Pass: 38 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```