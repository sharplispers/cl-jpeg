(cl:in-package :jpeg)

(defun test-reencode (&optional (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (time (multiple-value-bind (buf h w nc)
	    (jpeg:decode-image pathname)
	  (jpeg:encode-image #P"/tmp/test.jpg" buf nc h w))))

(defun test-reencode-prealloc (&optional (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (multiple-value-bind (h w ncomp)
      (jpeg-file-dimensions pathname)
    (let ((buf (allocate-buffer h w ncomp)))
      (decode-image pathname buf)
      (encode-image #P"/tmp/test.jpg" buf ncomp h w))))

(defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))

(defun test-encode-grayscale (&optional (output #P"/tmp/gray.jpg"))
  (let ((width 32)
        (height 32))
    (let ((buf (make-array (* width height) :initial-element 42 :element-type 'jpeg::uint8)))
      (time (jpeg:encode-image output buf 1 width height
                          :q-tabs *gray-q-tabs*)))))
