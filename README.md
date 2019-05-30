# GUETOR

### _Panji Kusuma <epanji@gmail.com>_

Guetor is package to guess selector for contents wrapper.

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

Running test suite GUETOR
 Running test NEW-DOCUMENT-SELECTOR .
 Running test NEW-DOCUMENT-LAST-SELECTOR ..
 Running test OLD-DOCUMENT-LAST-SELECTOR .
 Running test OLD-DOCUMENT-CUSTOM-SELECTOR ..
 Running test NEW-DOCUMENT-CUSTOM-SELECTOR ..
 Running test NEW-DOCUMENT-FULL-MODE-SELECTOR ..
 Running test NEW-DOCUMENT-DEFAULT-SELECTOR .
 Did 11 checks.
    Pass: 11 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```