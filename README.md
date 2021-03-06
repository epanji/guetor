GUETOR: Guess Selector
======================

GUETOR is system to guess selector for contents wrapper in HTML.
It provide utility to automatically guess or manually set CSS selector for contents wrapper.
It will retrieve Plump DOM as result that could be converted to desired format.

The files that constitute GUETOR are:

* [Interface](interface.lisp)
  provide internal and external symbol for GUETOR system.

* [Condition](condition.lisp)
  provide internal conditions for GUETOR system.

* [Content](content.lisp)
  provide automatic selector, custom selector and contents wrapper retriever.

* [Title](title.lisp)
  provide title retriever for contents wrapper.

* [Output](output.lisp)
  provide system to be inherited by others for lossy data conversion.

* [Navigation](navigation.lisp)
  provide navigation text retriever for url forward or backward direction.

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

Get url string for forward or backward navigation.

``` common-lisp
(let ((guetor:*navigation-base* "http://sample.com"))
  (guetor:find-navigation (guetor:document #p"test.html")))
=> "http://sample.com/next"

(let ((guetor:*navigation-base* "http://sample.com")
      (guetor:*navigation-direction* :forward))
  (guetor:find-navigation (guetor:document #p"test.html")))
=> "http://sample.com/next"

(let ((guetor:*navigation-base* "http://sample.com"))
  (guetor:find-navigation (guetor:document #p"test.html") :forward))
=> "http://sample.com/next"

(let ((guetor:*navigation-base* "http://sample.com")
      (guetor:*navigation-direction* :backward))
  (guetor:find-navigation (guetor:document #p"test.html")))
=> "http://sample.com/prev"

(let ((guetor:*navigation-base* "http://sample.com"))
  (guetor:find-navigation (guetor:document #p"test.html") :backward))
=> "http://sample.com/prev"
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

Convert to markdown by GUETOR. Animation below may become unrelevant in the future. It purposefully to show what we can do with GUETOR. Since we can extend GUETOR/OUTPUT to our system, we can freely define output with our custom configuration.

![Animation](compare-md.gif)

Author
------

Panji Kusuma (epanji@gmail.com)

_Notes:_

_Always remember when converting to desired format, it will not be able to convert back with the same result as before. (lossy)_

Tests
-----

``` common-lisp
CL-USER> (with-open-file (stream #p"tests-result.txt"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (let ((*standard-output* stream))
             (format stream "<pre>~&")
             (asdf:test-system "guetor")
             (format stream "~%</pre>")))
=> NIL
```

_Open tests result [HERE](tests-result.txt)._
