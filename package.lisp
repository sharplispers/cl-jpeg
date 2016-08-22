(defpackage #:jpeg
  (:use #:common-lisp)
  (:nicknames #:cl-jpeg)
  (:export #:encode-image
           #:decode-stream
           #:decode-image
	   #:allocate-buffer
	   #:jpeg-file-dimensions
           #:jpeg-to-bmp))
