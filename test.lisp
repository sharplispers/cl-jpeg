(in-package #:jpeg)

(time (multiple-value-bind (buf h w nc)
	  (decode-image #P"/home/eugene/Pictures/nyc.jpg")
	(encode-image #P"/tmp/test.jpg" buf nc h w)))
