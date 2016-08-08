(defpackage #:jpeg
  (:use #:common-lisp)
  (:nicknames #:cl-jpeg)
  (:export #:encode-image
           #:decode-stream
           #:decode-stream-height-width
           #:decode-image
	   #:jpeg-file-dimensions
           #:jpeg-to-bmp))
