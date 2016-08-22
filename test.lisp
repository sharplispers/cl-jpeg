(in-package #:jpeg)

(defun test-reencode (&optional (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (time (multiple-value-bind (buf h w nc)
	    (decode-image pathname)
	  (encode-image #P"/tmp/test.jpg" buf nc h w))))

(defun test-reencode-prealloc (&optional (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (multiple-value-bind (h w ncomp)
      (jpeg-file-dimensions pathname)
    (let ((buf (allocate-buffer h w ncomp)))
      (decode-image pathname buf)
      (encode-image #P"/tmp/test.jpg" buf ncomp h w))))
