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
 Did 24 checks.
    Pass: 24 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```