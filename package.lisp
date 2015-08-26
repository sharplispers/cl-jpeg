(defpackage #:jpeg
  (:use #:common-lisp)
  (:export #:encode-image
           #:decode-stream
           #:decode-stream-height-width
           #:decode-image
           #:jpeg-to-bmp))
