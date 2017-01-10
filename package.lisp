(defpackage #:jpeg
  (:use #:common-lisp)
  (:nicknames #:cl-jpeg)
  (:export #:encode-image
           #:decode-stream
           #:decode-image
	   #:jpeg-error #:jpeg-encoder-error #:jpeg-decoder-error #:unsupported-jpeg-format #:unrecognized-file-format
	   #:allocate-buffer
	   #:jpeg-file-dimensions
           #:jpeg-to-bmp))
