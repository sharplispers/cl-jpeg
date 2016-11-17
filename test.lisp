(cl:defpackage :jpeg-test
  (:use #:cl #:jpeg))

(cl:in-package :jpeg-test)

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

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "cl-jpeg"))))

(ensure-directories-exist (output-image ""))

(defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))

(defun test-encode (&optional (output (test-image "simple.jpeg")))
  (let ((width 32)
        (height 32))
    (let ((buf (make-array (* width height) :initial-element 42)))
      (time (jpeg:encode-image output buf 1 width height
                          :q-tabs *gray-q-tabs*)))))
