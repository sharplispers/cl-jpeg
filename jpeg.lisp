;;  -*- Mode: LISP; Package: (JPEG :use (common-lisp)) -*-
;;; Generic Common Lisp JPEG encoder/decoder implementation
;;; $Id: jpeg.lisp,v 1.6 2011-03-14 22:49:00 charmon Exp $
;;; Version 1.3, August 2015
;;; Written by Eugene Zaikonnikov [eugene@funcall.org]
;;; Copyright [c] 1999,2015, Eugene Zaikonnikov <eugene@funcall.org>
;;;               
;;; This software is distributed under the terms of BSD-like license
;;; [see LICENSE for details]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This software was sponsored by Kelly E. Murray
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Two main functions available:
;;;
;;; (encode-image filename image ncomp h w &key sampling q-tabs q-factor), where:
;;; filename - output file name
;;; ncomp - number of components (1-4)
;;; h, w - source image height and width respectively
;;; image - array of B, G, R pixels in case of three component image,
;;;         array of grayscale pixels in case of single component,
;;;         array of 2 or 4 pixels in the case of two or four component image respectively
;;; :q-tabs - specifies quantization tables vector, should be 1 for 1,
;;;           2 for 2, 2 for 3 and 4 entries for 4 components
;;; :sampling - sampling frequency for ncomp components by X and Y axis,
;;;             e.g. '((2 2) (1 1) (1 1)) for three components, can be omitted
;;;             for grayscale and RGB images
;;; :q-factor - quality specifier (1-64), default is 64
;;; Returns nothing of practical use
;;;
;;; (decode-image filename)
;;; filename - jpeg file name
;;; Returns (multiple-valued) IMAGE array in the same format as encoder source image,
;;; image HEIGHT and image WIDTH
;;;
;;; For those impatient additional function defined:
;;; (jpeg-to-bmp &key infile outfile)
;;; Converts JPEG image specified by infile into Microsoft Windows 24-bit BMP format (outfile),
;;; returns NIL
;;;
;;; Additionaly, you may use more user-friendly version of encode-image: encode-wrapper.
;;; (encoding-wrapper filename image ncomp h w &key quality)
;;; All parameters have the same meaning as in encode-image, except quality.
;;; It is an integer value ranging 1 to 5 which specifies
;;; subjective quality of a resulting image.

;;; Technical details: encoder produces interleaved jpeg file, without restarts.
;;; In a case of 3 components image will be written in JFIF format.

;;; Decoder can deal with *almost* all baseline jpeg files, regardless JFIF or not.

;;; It supports restarts, interleaved/noninterleaved files, multiscan images, 1 to 4 color
;;; channels, up to 4 quantization tables and two sets of huffman tables with random order
;;; of their definition inside the image. Decoder *does not* support DNL marker, due to
;;; it's rarity and amount of work needed to implement it, so decoder isn't baseline in a
;;; strict sense.

;;; Both encoder and decoder utilize Loeffer, Ligtenberg and Moschytz integer discrete
;;; cosine transform algorithms with 12 multiplications in each loop.

;;; Based on CCITT Rec. T.81
;;; "Information technology - digital compression and coding of continious-tone still images
;;; - requirements and guidelines".
;;; Credits:
;;; to the Independent JPEG Group -
;;; colorspace conversion and DCT algorithms were adopted from their sources;
;;; to Jeff Dalton for his wise paper "Common Lisp Pitfalls".

(in-package #:jpeg)

(declaim (inline csize write-stuffed quantize get-average zigzag encode-block
                 llm-dct descale crunch colorspace-convert subsample inverse-llm-dct
                 dequantize upsample extend recieve decode-ac decode-dc decode-block
                 izigzag write-bits))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimize*  '(optimize (safety 1) (space 0) (debug 0) (speed 3))))
;    '(optimize (safety 1) (space 3) (debug 0) (speed 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; For ease of reference
(defmacro dbref (data x y)
  `(the fixnum (aref (aref ,data ,y) ,x)))

;;; Integer arithmetic wrappers
(defmacro plus (a b)
  `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))

(defmacro minus (a b)
  #+(or clisp abcl)
  `(- ,a ,b)
  #-(or clisp abcl)
  `(the fixnum (- (the fixnum ,a) (the fixnum ,b))))

(defmacro mul (a b)
  `(the fixnum (* (the fixnum ,a) (the fixnum ,b)))))

;;; Somewhat silly, but who knows...
(when (/= (integer-length most-positive-fixnum)
          (integer-length most-negative-fixnum))
  (error "Can't compile with asymmetric fixnums!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here we define some constants (markers, quantization and huffman tables etc.)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(deftype uint8 () '(unsigned-byte 8))

(eval-when (:compile-toplevel :load-toplevel)

(defun 2d-uint8-array (&rest contents)
  (let ((nrow (length contents)))
    (make-array nrow :initial-contents
                (loop for row in contents
                      collecting (make-array (length row) :element-type 'uint8
                                             :initial-contents row)))))

;;; Source huffman tables for the encoder
(define-constant +luminance-dc-bits+
  #(#x00 #x01 #x05 #x01 #x01 #x01 #x01 #x01
     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define-constant +luminance-dc-values+
  #(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +chrominance-dc-bits+
  #(#x00 #x03 #x01 #x01 #x01 #x01 #x01 #x01
     #x01 #x01 #x01 #x00 #x00 #x00 #x00 #x00))

(define-constant +chrominance-dc-values+
  #(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +luminance-ac-bits+
  #(#x00 #x02 #x01 #x03 #x03 #x02 #x04 #x03
     #x05 #x05 #x04 #x04 #x00 #x00 #x01 #x7d))

(define-constant +luminance-ac-values+
  #(#x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12
     #x21 #x31 #x41 #x06 #x13 #x51 #x61 #x07
     #x22 #x71 #x14 #x32 #x81 #x91 #xa1 #x08
     #x23 #x42 #xb1 #xc1 #x15 #x52 #xd1 #xf0
     #x24 #x33 #x62 #x72 #x82 #x09 #x0a #x16
     #x17 #x18 #x19 #x1a #x25 #x26 #x27 #x28
     #x29 #x2a #x34 #x35 #x36 #x37 #x38 #x39
     #x3a #x43 #x44 #x45 #x46 #x47 #x48 #x49
     #x4a #x53 #x54 #x55 #x56 #x57 #x58 #x59
     #x5a #x63 #x64 #x65 #x66 #x67 #x68 #x69
     #x6a #x73 #x74 #x75 #x76 #x77 #x78 #x79
     #x7a #x83 #x84 #x85 #x86 #x87 #x88 #x89
     #x8a #x92 #x93 #x94 #x95 #x96 #x97 #x98
     #x99 #x9a #xa2 #xa3 #xa4 #xa5 #xa6 #xa7
     #xa8 #xa9 #xaa #xb2 #xb3 #xb4 #xb5 #xb6
     #xb7 #xb8 #xb9 #xba #xc2 #xc3 #xc4 #xc5
     #xc6 #xc7 #xc8 #xc9 #xca #xd2 #xd3 #xd4
     #xd5 #xd6 #xd7 #xd8 #xd9 #xda #xe1 #xe2
     #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea
     #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

(define-constant +chrominance-ac-bits+
  #(#x00 #x02 #x01 #x02 #x04 #x04 #x03 #x04
     #x07 #x05 #x04 #x04 #x00 #x01 #x02 #x77))

(define-constant +chrominance-ac-values+
  #(#x00 #x01 #x02 #x03 #x11 #x04 #x05 #x21
     #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71
     #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91
     #xa1 #xb1 #xc1 #x09 #x23 #x33 #x52 #xf0
     #x15 #x62 #x72 #xd1 #x0a #x16 #x24 #x34
     #xe1 #x25 #xf1 #x17 #x18 #x19 #x1a #x26
     #x27 #x28 #x29 #x2a #x35 #x36 #x37 #x38
     #x39 #x3a #x43 #x44 #x45 #x46 #x47 #x48
     #x49 #x4a #x53 #x54 #x55 #x56 #x57 #x58
     #x59 #x5a #x63 #x64 #x65 #x66 #x67 #x68
     #x69 #x6a #x73 #x74 #x75 #x76 #x77 #x78
     #x79 #x7a #x82 #x83 #x84 #x85 #x86 #x87
     #x88 #x89 #x8a #x92 #x93 #x94 #x95 #x96
     #x97 #x98 #x99 #x9a #xa2 #xa3 #xa4 #xa5
     #xa6 #xa7 #xa8 #xa9 #xaa #xb2 #xb3 #xb4
     #xb5 #xb6 #xb7 #xb8 #xb9 #xba #xc2 #xc3
     #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xd2
     #xd3 #xd4 #xd5 #xd6 #xd7 #xd8 #xd9 #xda
     #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9
     #xea #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

;;;Zigzag encoding matrix
(define-constant +zigzag-index+
  (2d-uint8-array '(0  1  5  6 14 15 27 28)
                  '(2  4  7 13 16 26 29 42)
                  '(3  8 12 17 25 30 41 43)
                  '(9 11 18 24 31 40 44 53)
                  '(10 19 23 32 39 45 52 54)
                  '(20 22 33 38 46 51 55 60)
                  '(21 34 37 47 50 56 59 61)
                  '(35 36 48 49 57 58 62 63)))

;;;JPEG file markers
(defconstant +M_COM+ #xfe)
(defconstant +M_SOF0+ #xc0)
(defconstant +M_SOF2+ #xc2)
(defconstant +M_DHT+ #xc4)
(defconstant +M_RST0+ #xd0)
(defconstant +M_RST7+ #xd7)
(defconstant +M_SOI+ #xd8)
(defconstant +M_EOI+ #xd9)
(defconstant +M_SOS+ #xda)
(defconstant +M_DQT+ #xdb)
(defconstant +M_DNL+ #xdc)
(defconstant +M_DRI+ #xdd)
(defconstant +M_DAC+ #xcc)
(defconstant +M_APP0+ #xe0)

;;; Default quantization tables
(define-constant +q-luminance+
  (2d-uint8-array '(16 11 10 16 24 40 51 61)
                  '(12 12 14 19 26 58 60 55)
                  '(14 13 16 24 40 57 69 56)
                  '(14 17 22 29 51 87 80 62)
                  '(18 22 37 56 68 109 103 77)
                  '(24 35 55 64 81 104 113 92)
                  '(49 64 78 87 103 121 120 101)
                  '(72 92 95 98 112 100 103 99)))

(define-constant +q-chrominance+
  (2d-uint8-array '(17 18 24 47 99 99 99 99)
                  '(18 21 26 66 99 99 99 99)
                  '(24 26 56 99 99 99 99 99)
                  '(47 66 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)))

(define-constant +q-luminance-hi+
  (2d-uint8-array '(10 7 6 10 15 25 32 38)
                  '(8 8 9 12 16 36 38 34)
                  '(9 8 10 15 25 36 43 35)
                  '(9 11 14 18 32 54 50 39)
                  '(11 14 23 35 42 68 64 48)
                  '(15 22 34 40 51 65 71 58)
                  '(31 40 49 54 64 76 75 63)
                  '(45 58 59 61 70 62 64 62)))

(define-constant +q-chrominance-hi+
  (2d-uint8-array '(11 11 15 29 62 62 62 62)
                  '(11 13 16 41 62 62 62 62)
                  '(15 16 35 62 62 62 62 62)
                  '(29 41 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)))

)

;;; Quantization performance test, each branch quantizes 30000 random matrixes
(eval-when (:compile-toplevel)

  (format t "Performing compile-time optimization.. please wait.~%")
  (finish-output)
  
  (defvar *quantize-optimization*
    (<= (let ((time1 (get-internal-run-time)))
	  (loop for i fixnum from 1 to 30000 do
		(loop for row across +q-luminance+ do
		      (loop for q-coef fixnum across row
			    maximize (round (random 128) q-coef))))
	  (minus (get-internal-run-time) time1))
	(let ((time1 (get-internal-run-time)))
	  (loop for i fixnum from 1 to 30000 do
		(loop for q-row across +q-luminance+ do
		      (loop for val fixnum = (random 128)
			    for absval fixnum = (abs val)
			    for qc fixnum across q-row
			    maximize
			    (cond ((< absval (ash qc -1))
				   0)
				  ((<= absval qc)
				   (if (minusp val)
				       -1
				     1))
				  ((<= (ash absval -1) qc)
				   (if (zerop (logand absval 1))
				       (if (minusp val)
					   -1
					 1)
				     (if (minusp val)
					 -2
				       2)))
				  (t
				   (round val qc))))))
	  (minus (get-internal-run-time) time1))))
  (format t "Done.~%")
  (finish-output))

(define-constant +q-tables+ (vector +q-luminance+ +q-chrominance+))

;;; This table is used to map coefficients into SSSS value
(define-constant +csize+ (make-array 2047
                                 :initial-contents
                                 (loop for i fixnum from 0 to 2046
                                       collecting (integer-length (abs (minus i 1023))))))

;;; Some constants for colorspace mapper
(defconstant shift (1- (integer-length (ash most-positive-fixnum -7))))
(defconstant +.299+ (round (+ (* 0.299 (ash 1 shift)) 0.5)))
(defconstant +.587+ (round (+ (* 0.587 (ash 1 shift)) 0.5)))
(defconstant +.114+ (round (+ (* 0.114 (ash 1 shift)) 0.5)))
(defconstant +-.1687+ (round (+ (* -0.1687 (ash 1 shift)) 0.5)))
(defconstant +-.3313+ (round (+ (* -0.3313 (ash 1 shift)) 0.5)))
(defconstant +-.4187+ (round (+ (* -0.4187 (ash 1 shift)) 0.5)))
(defconstant +-.0813+ (round (+ (* -0.0813 (ash 1 shift)) 0.5)))
(defconstant +.5+ (round (+ (* 0.5 (ash 1 shift)) 0.5)))
(defconstant +uvoffset+ (ash 128 shift))
(defconstant +one-half+ (1- (ash 1 (1- shift))))
(defconstant +r-y-off+ 0)
(defconstant +g-y-off+ 256)
(defconstant +b-y-off+ (* 2 256))
(defconstant +r-u-off+ (* 3 256))
(defconstant +g-u-off+ (* 4 256))
(defconstant +b-u-off+ (* 5 256))
(defconstant +r-v-off+ +b-u-off+)
(defconstant +g-v-off+ (* 6 256))
(defconstant +b-v-off+ (* 7 256))

;;;Direct color conversion table
(defvar *ctab* (make-array 2048 :initial-element 0))

;;; Filling in the table
(loop for i fixnum from 0 to 255 do
      (setf (svref *ctab* (plus i +r-y-off+))
            (mul +.299+ i))
      (setf (svref *ctab* (plus i +g-y-off+))
            (mul +.587+ i))
      (setf (svref *ctab* (plus i +b-y-off+))
            (mul +.114+ i))
      (setf (svref *ctab* (plus i +r-u-off+))
            (mul +-.1687+ i))
      (setf (svref *ctab* (plus i +g-u-off+))
            (mul +-.3313+ i))
      (setf (svref *ctab* (plus i +b-u-off+))
            (+ (mul +.5+ i) +uvoffset+ +one-half+))
      (setf (svref *ctab* (plus i +r-v-off+))
            (+ (mul +.5+ i) +uvoffset+ +one-half+))
      (setf (svref *ctab* (plus i +g-v-off+))
            (mul +-.4187+ i))
      (setf (svref *ctab* (plus i +b-v-off+))
            (mul +-.0813+ i)))

;;; Constantsants for the inverse colorspace conversion
(defconstant +1.40200+ (round (+ (* 1.40200 (ash 1 shift)) 0.5)))
(defconstant +1.77200+ (round (+ (* 1.77200 (ash 1 shift)) 0.5)))
(defconstant +-0.71414+ (round (+ (* -0.71414 (ash 1 shift)) 0.5)))
(defconstant +-0.34414+ (round (+ (* -0.34414 (ash 1 shift)) 0.5)))

;;; Inverse color conversion tables
(defvar *cr-r-tab* (make-array 256))
(defvar *cb-g-tab* (make-array 256))
(defvar *cr-g-tab* (make-array 256))
(defvar *cb-b-tab* (make-array 256))

;;; Filling up the tables
(loop for i from 0 to 255
      for x from -127 do
      (setf (svref *cr-r-tab* i) (ash (plus (mul +1.40200+ x) +one-half+) (- shift)))
      (setf (svref *cb-b-tab* i) (ash (plus (mul +1.77200+ x) +one-half+) (- shift)))
      (setf (svref *cr-g-tab* i) (mul +-0.71414+ x))
      (setf (svref *cb-g-tab* i) (plus (mul +-0.34414+ x) +one-half+)))

;;; Constants for LLM DCT
(defconstant dct-shift  ; defining DCT scaling
  (if (<= (integer-length most-positive-fixnum) 31)
      (minus 13 (round (minus 31 (integer-length most-positive-fixnum)) 2))
    13))

(defconstant +shift-1+ (1- dct-shift))
(defconstant +shift+1+ (1+ dct-shift))
(defconstant +shift+4+ (+ dct-shift 4))
(defconstant +FIX-0-298631336+ (round (+ (* 0.298631336 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-390180644+ (round (+ (* 0.390180644 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-541196100+ (round (+ (* 0.541196100 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-765366865+ (round (+ (* 0.765366865 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-899976223+ (round (+ (* 0.899976223 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-175875602+ (round (+ (* 1.175875602 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-501321110+ (round (+ (* 1.501321110 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-847759065+ (round (+ (* 1.847759065 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-961570560+ (round (+ (* 1.961570560 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-053119869+ (round (+ (* 2.053119869 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-562915447+ (round (+ (* 2.562915447 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-3-072711026+ (round (+ (* 3.072711026 (ash 1 dct-shift)) 0.5)))

;;; Post-IDCT limiting array
(defvar *idct-limit-array* (make-array 512 :initial-element 0 :element-type 'uint8))
(loop for n from 0
      for i from 128 to 383 do
      (setf (aref *idct-limit-array* i) n))
(loop for i from 384 to 511 do
      (setf (aref *idct-limit-array* i) 255))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encoder part

;;; Subsamples inbuf into outbuf
(defun subsample (inbuf outbuf H V xlim ylim iH iV)
  (declare #.*optimize*
           (type fixnum H V xlim ylim iV iH)
           (type (simple-vector *) inbuf outbuf))
  (loop for by fixnum from 0 below V do
        (loop for bx fixnum from 0 below H
              for block = (svref outbuf (plus bx (mul by H))) do
              (loop for y fixnum from (ash by 3) by iV
                    for yp fixnum from 0 to 7 do
                    (loop for x fixnum from (ash bx 3) by iH
                          for xp fixnum from 0 to 7 do
                          (setf (dbref block xp yp)
                                (the fixnum (cond ((and (<= x xlim) (<= y ylim))
                                                   (dbref inbuf x y))
                                                  ((and (> x xlim) (> y ylim))
                                                   (dbref inbuf xlim ylim))
                                                  ((> x xlim)
                                                   (dbref inbuf xlim y))
                                                  ((> y ylim)
                                                   (dbref inbuf x ylim))
                                                  (t
                                                   (error "Internal error"))
                                                   ))))))))

;;; Returns sum of Vi*Hi
(defun count-relation (smp)
  (loop for entry in smp
        summing (mul (first entry) (second entry))))

;;; Cutting specified part of image (used for non-RGB images)
(defun crop-image (inbuf outbuf dx dy h w height width ncomp)
  (let ((xend (plus dx (1- width)))
        (yend (plus dy (1- height))))
    (declare #.*optimize*
             (type fixnum dx dy h w height width ncomp xend yend)
             (type (simple-array uint8 (*)) inbuf outbuf))
    (setf xend (min xend (1- w)))
    (setf yend (min yend (1- h)))
    (loop for yd fixnum from dy to yend
          for ypos fixnum = (* w yd ncomp) do
          (loop for xd fixnum from dx to xend
                for pos fixnum = (plus (mul xd ncomp) ypos)
                for cx fixnum = (minus xd dx)
                for cy fixnum = (minus yd dy) do
                (loop for i fixnum from 0 below ncomp do
                      (setf (dbref (aref outbuf i) cx cy)
                            (minus (aref inbuf (plus pos i)) 128)))))
    (values xend yend)))

;;; Direct color mapping
(defun colorspace-convert (RGB YUV dx dy h w height width)
  (let ((xend (plus dx (1- width)))
        (yend (plus dy (1- height)))
        (Y (aref YUV 0))
        (U (aref YUV 1))
        (V (aref YUV 2)))
    (declare #.*optimize*
             (type fixnum dx dy h w height width xend yend)
             (type (simple-vector *) YUV Y U V)
             (type (simple-array uint8 (*)) RGB))
    (setf xend (min xend (1- w)))
    (setf yend (min yend (1- h)))
    (loop for yd fixnum from dy to yend
          for ypos fixnum = (* w yd 3) do
          (loop for xd fixnum from dx to xend
                for pos fixnum = (plus (mul xd 3) ypos)
                for r fixnum = (aref rgb (plus pos 2))
                for g fixnum = (aref rgb (1+ pos))
                for b fixnum = (aref rgb pos)
                for cx fixnum = (minus xd dx)
                for cy fixnum = (minus yd dy) do
                (setf (dbref Y cx cy) (minus (ash (+ (svref *ctab* (plus r +r-y-off+))
                                                          (svref *ctab* (plus g +g-y-off+))
                                                          (svref *ctab* (plus b +b-y-off+)))
                                                       (- shift))
                                             128))
                (setf (dbref U cx cy) (minus (ash (+ (svref *ctab* (plus r +r-u-off+))
                                                          (svref *ctab* (plus g +g-u-off+))
                                                          (svref *ctab* (plus b +b-u-off+)))
                                                       (- shift))
                                             128))
                (setf (dbref V cx cy) (minus (ash (+ (svref *ctab* (plus r +r-v-off+))
                                                          (svref *ctab* (plus g +g-v-off+))
                                                          (svref *ctab* (plus b +b-v-off+)))
                                                       (- shift))
                                             128))))
    (values xend yend)))

;;; Converts given image sampling into frequencies of pixels of components
(defun convert-sampling (s Hmax Vmax)
  (declare (type fixnum Hmax Vmax))
  (make-array (length s)
              :initial-contents (loop for entry in s
                                      collecting (list (the fixnum (/ Hmax (first entry)))
                                                       (the fixnum (/ Vmax (second entry)))))))

;;; Quantization (also removes factor of 8 after DCT)

(defmacro quantize-block ()
  (if *quantize-optimization*
      '(loop for block-row across block
             for q-row across q-table do
             (loop for x fixnum from 0 to 7
                   for val fixnum = (ash (svref block-row x) -3)
                   for qc fixnum = (aref q-row x) do
                   (setf (svref block-row x) (the fixnum (round val qc)))))
    '(loop for block-row across block
           for q-row across q-table do
           (loop for x fixnum from 0 to 7
                 for val fixnum = (ash (svref block-row x) -3)
                 for absval fixnum = (abs val)
                 for qc fixnum = (aref q-row x) do
                 (cond ((< absval (ash qc -1))
                        ;; you won't believe, but under LWW 4.1 such ugly hack gives
                        ;; very sufficient speedup
                        (setf (svref block-row x) 0))
                       ((<= absval qc)
                        (if (minusp val)
                            (setf (svref block-row x) -1)
                         (setf (svref block-row x) 1)))
                       ((<= (ash absval -1) qc)
                        (if (zerop (logand absval 1))
                            (if (minusp val)
                                (setf (svref block-row x) -1)
                              (setf (svref block-row x) 1))
                          (if (minusp val)
                              (setf (svref block-row x) -2)
                            (setf (svref block-row x) 2))))
                       (t
                        (setf (svref block-row x) (the fixnum (round val qc)))))))))

(defun quantize (block q-table)
  (declare #.*optimize* (type (simple-vector *) block q-table))
  (quantize-block))

;;; LLM DCT aux definitions
(defun descale (x n)
  (declare #.*optimize* (type fixnum x n))
  (the fixnum (ash (plus x (ash 1 (1- n))) (- n))))

(defmacro plus3 (x y z)
  `(plus (plus ,x ,y) ,z))

;;; Implementation of Loeffer, Ligtenberg and Moschytz forward DCT
(defun llm-dct (data)
  (declare #.*optimize* (type (simple-vector *) data))
  (loop with tmp0 fixnum and tmp1 fixnum and tmp2 fixnum
        and tmp3 fixnum and tmp4 fixnum and tmp5 fixnum
        and tmp6 fixnum and tmp7 fixnum and tmp10 fixnum
        and tmp11 fixnum and tmp12 fixnum and tmp13 fixnum
        and z1 fixnum and z2 fixnum and z3 fixnum
        and z4 fixnum and z5 fixnum do
        (loop ; for dptrpos fixnum from 7 downto 0
              ; for dptr = (svref data dptrpos) do
              for dptr across data do   ; iterating over rows
              (setf tmp0 (plus (svref dptr 0) (svref dptr 7)))
              (setf tmp7 (minus (svref dptr 0) (svref dptr 7)))
              (setf tmp1 (plus (svref dptr 1) (svref dptr 6)))
              (setf tmp6 (minus (svref dptr 1) (svref dptr 6)))
              (setf tmp2 (plus (svref dptr 2) (svref dptr 5)))
              (setf tmp5 (minus (svref dptr 2) (svref dptr 5)))
              (setf tmp3 (plus (svref dptr 3) (svref dptr 4)))
              (setf tmp4 (minus (svref dptr 3) (svref dptr 4)))
              (setf tmp10 (plus tmp0 tmp3))
              (setf tmp13 (minus tmp0 tmp3))
              (setf tmp11 (plus tmp1 tmp2))
              (setf tmp12 (minus tmp1 tmp2))
              (setf (svref dptr 0) (ash (plus tmp10 tmp11) 1))
              (setf (svref dptr 4) (ash (minus tmp10 tmp11) 1))
              (setf z1 (mul (plus tmp12 tmp13) +FIX-0-541196100+))
              (setf (svref dptr 2) (descale (plus z1 (mul tmp13 +FIX-0-765366865+)) +shift-1+))
              (setf (svref dptr 6) (descale (plus z1 (mul tmp12 (- +FIX-1-847759065+))) +shift-1+))
              (setf z1 (plus tmp4 tmp7))
              (setf z2 (plus tmp5 tmp6))
              (setf z3 (plus tmp4 tmp6))
              (setf z4 (plus tmp5 tmp7))
              (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
              (setf tmp4 (mul tmp4 +fix-0-298631336+))
              (setf tmp5 (mul tmp5 +fix-2-053119869+))
              (setf tmp6 (mul tmp6 +fix-3-072711026+))
              (setf tmp7 (mul tmp7 +fix-1-501321110+))
              (setf z1 (mul z1 (- +fix-0-899976223+)))
              (setf z2 (mul z2 (- +fix-2-562915447+)))
              (setf z3 (mul z3 (- +fix-1-961570560+)))
              (setf z4 (mul z4 (- +fix-0-390180644+)))
              (incf z3 z5)
              (incf z4 z5)
              (setf (svref dptr 7) (descale (plus3 tmp4 z1 z3) +shift-1+))
              (setf (svref dptr 5) (descale (plus3 tmp5 z2 z4) +shift-1+))
              (setf (svref dptr 3) (descale (plus3 tmp6 z2 z3) +shift-1+))
              (setf (svref dptr 1) (descale (plus3 tmp7 z1 z4) +shift-1+)))
        (loop for cnt fixnum from 7 downto 0 do ; second pass: on columns
              (setf tmp0 (plus (dbref data cnt 0) (dbref data cnt 7)))
              (setf tmp7 (minus (dbref data cnt 0) (dbref data cnt 7)))
              (setf tmp1 (plus (dbref data cnt 1) (dbref data cnt 6)))
              (setf tmp6 (minus (dbref data cnt 1) (dbref data cnt 6)))
              (setf tmp2 (plus (dbref data cnt 2) (dbref data cnt 5)))
              (setf tmp5 (minus (dbref data cnt 2) (dbref data cnt 5)))
              (setf tmp3 (plus (dbref data cnt 3) (dbref data cnt 4)))
              (setf tmp4 (minus (dbref data cnt 3) (dbref data cnt 4)))
              (setf tmp10 (plus tmp0 tmp3))
              (setf tmp13 (minus tmp0 tmp3))
              (setf tmp11 (plus tmp1 tmp2))
              (setf tmp12 (minus tmp1 tmp2))
              (setf (dbref data cnt 0) (descale (plus tmp10 tmp11) 1))
              (setf (dbref data cnt 4) (descale (minus tmp10 tmp11) 1))
              (setf z1 (mul (plus tmp12 tmp13) +fix-0-541196100+))
              (setf (dbref data cnt 2) (descale (plus z1 (mul tmp13 +fix-0-765366865+)) +shift+1+))
              (setf (dbref data cnt 6) (descale (plus z1 (mul tmp12 (- +fix-1-847759065+))) +shift+1+))
              (setf z1 (plus tmp4 tmp7))
              (setf z2 (plus tmp5 tmp6))
              (setf z3 (plus tmp4 tmp6))
              (setf z4 (plus tmp5 tmp7))
              (setf z5 (mul (plus z3 z4) +fix-1-175875602+))
              (setf tmp4 (mul tmp4 +fix-0-298631336+))
              (setf tmp5 (mul tmp5 +fix-2-053119869+))
              (setf tmp6 (mul tmp6 +fix-3-072711026+))
              (setf tmp7 (mul tmp7 +fix-1-501321110+))
              (setf z1 (mul z1 (- +fix-0-899976223+)))
              (setf z2 (mul z2 (- +fix-2-562915447+)))
              (setf z3 (mul z3 (- +fix-1-961570560+)))
              (setf z4 (mul z4 (- +fix-0-390180644+)))
              (incf z3 z5)
              (incf z4 z5)
              (setf (dbref data cnt 7) (descale (plus3 tmp4 z1 z3) +shift+1+))
              (setf (dbref data cnt 5) (descale (plus3 tmp5 z2 z4) +shift+1+))
              (setf (dbref data cnt 3) (descale (plus3 tmp6 z2 z3) +shift+1+))
              (setf (dbref data cnt 1) (descale (plus3 tmp7 z1 z4) +shift+1+)))
        (return)))

;;; Forward DCT and quantization
(defun crunch (buf pos table)
  (declare #.*optimize*
           (type fixnum pos)
           (type (simple-vector *) buf))
  (llm-dct (svref buf pos))
  (quantize (svref buf pos) table))

;;; Q-tables scaling
(defun q-scale (table q-factor)
  (declare #.*optimize*)
  (when (/= q-factor 64)
    (let ((factor (/ q-factor 64)))
      (loop for q-row across table do
            (loop for x fixnum from 0 to 7 do
                  (setf (svref q-row x)
                        (the fixnum (round (* (svref q-row x) factor)))))))))

;;; Function that maps value into SSSS
(defun csize (n)
    (declare #.*optimize* (type fixnum n))
    (svref +csize+ (plus n 1023)))

;;; zigzag ordering
(defun zigzag (buffer)
  (declare #.*optimize* (type (simple-vector 8) buffer))
  (loop for row across buffer
        for z-row across +zigzag-index+ do
        (loop for x fixnum from 0 to 7 do
              (setf (aref *zz-result* (aref z-row x))
                    (the fixnum (aref row x)))))
  *zz-result*)

;;; Writes frame header
(defun write-frame-header (maxX maxY cn q-tables sampling tqv out-stream)
  (declare #.*optimize* (type fixnum maxX maxY cn))
  (write-huffman-tables out-stream)
  (write-quantization-tables q-tables out-stream)
  ;; writing frame header
  (write-marker +M_SOF0+ out-stream)
  (write-byte 0 out-stream) ; length
  (write-byte (plus 8 (mul 3 cn)) out-stream)
  (write-byte 8 out-stream) ; sample precision
  (write-byte (ash maxY -8) out-stream) ; max height
  (write-byte (logand maxY #xff) out-stream)
  (write-byte (ash maxX -8) out-stream) ; max width
  (write-byte (logand maxX #xff) out-stream)
  (write-byte cn out-stream) ; number of components
  (loop for entry in sampling
        for i fixnum from 0 by 1 do
        (write-byte i out-stream)
        (write-byte         ; H and V
         (deposit-field (second entry) (byte 4 0)(ash (first entry) 4))
         out-stream)
        (write-byte (svref tqv i) out-stream))) ; Tq

;;; Writes byte with stuffing (adds zero after FF code)
(defun write-stuffed (b s)
  (declare #.*optimize* (type fixnum b)
           (type stream s))
   (write-byte b s)
   (if (= b #xFF)
      (write-byte 0 s)))

;;; A function for bit streaming
;;; NB: probably it's a good idea to encapsulate this behavior into a class, but I'm
;;; afraid that method dispatch would be too slow

(defun write-bits (bi ni s)
  (declare #.*optimize*
           ;(special *prev-length* *prev-byte*)
           (type fixnum bi ni)
           (type stream s))
  (loop with lim fixnum = (if (> ni 8) 1 0)
        for i fixnum from lim downto 0 do
        (let ((b (ldb (byte 8 (ash i 3)) bi))
              (n (cond ((and (= i 1) (= ni 16)) 8)
                       ((and (= i 0) (/= lim 0)) 8)
                       ((= ni 8) 8)
                       (t (logand ni 7)))))
          (declare (type fixnum b n)
                   (dynamic-extent b n))
          (cond ((zerop n))
                ((>= (plus n *prev-length*) 8)
                 (let* ((result (ash *prev-byte* (minus 8 *prev-length*)))
                        (total-length (plus n *prev-length*))
                        (overflow (minus total-length 8)))
                   (declare (type fixnum overflow total-length result)
                            (dynamic-extent overflow total-length result))
                   (setf *prev-byte* (ldb (byte overflow 0) b))
                   (write-stuffed (deposit-field
                                   (ldb (byte (minus n overflow) overflow) b)
                                   (byte (minus 8 *prev-length*) 0)
                                   result)
                                  s)
                   (setf *prev-length* overflow)))
                (t  (setf *prev-byte* (deposit-field b (byte n 0) (ash *prev-byte* n)))
                    (incf *prev-length* n))))))

;;; Encodes block using specified huffman tables, returns new pred (DC prediction value)
;;; and last code written to stream for padding
(defun encode-block (block tables pred s)
  (declare #.*optimize* (type fixnum pred)
           (type (simple-vector *) block))
  (let* ((ehufsi-dc (first (first tables)))
         (ehufco-dc (second (first tables)))
         (ehufsi-ac (first (second tables)))
         (ehufco-ac (second (second tables)))
         (newpred (svref block 0))
         (diff (minus newpred pred))
         (dcpos (csize diff)))
    (declare (type fixnum pred newpred diff dcpos)
             (dynamic-extent diff dcpos))
    ;; writing dc code first
    (write-bits (svref ehufco-dc dcpos) (svref ehufsi-dc dcpos) s)
    (cond ((minusp diff) (write-bits (1- diff) (csize diff) s))
          (t (write-bits diff (csize diff) s)))
    ;; writing ac sequence
    (loop with r fixnum = 0 for k fixnum from 1 to 63 do
          (if (zerop (svref block k))
              (if (= k 63)
                  (progn
                    (write-bits (svref ehufco-ac 0) (svref ehufsi-ac 0) s) ; writing EOB
                    (return))
                (incf r))
            (progn
              (loop while (> r 15) do
                    (write-bits (svref ehufco-ac #xf0) (svref ehufsi-ac #xf0) s)
                    (decf r 16))
              (let* ((ssss (csize (svref block k)))
                     (rs (plus ssss (ash r 4))))
                (write-bits (svref ehufco-ac rs) (svref ehufsi-ac rs) s)
                (when (minusp (svref block k))
                  (decf (svref block k) 1))
                (write-bits (svref block k) ssss s))
              (setf r 0))))
    newpred))

;;; Emits q-tables
(defun write-quantization-tables (tables s)
  (let ((len (plus 2 (mul 65 (length tables)))))
    (write-marker +M_DQT+ s)
    (write-byte (ash len -8) s) ; MSB
    (write-byte (logand len #xff) s) ; LSB
    (loop for table across tables
          for i fixnum from 0 do
          (write-byte i s)
          (write-sequence (zigzag table) s))))

;;; Emits huffman tables in the following order:
;;; luminance DC
;;; luminance AC
;;; chrominance DC
;;; chrominance AC
(defun write-huffman-tables (s)
  (let ((len (+ 2 (* 17 4)
                (length +luminance-dc-values+)
                (length +luminance-ac-values+)
                (length +chrominance-dc-values+)
                (length +chrominance-ac-values+))))
    (write-marker +M_DHT+ s)
    (write-byte (ash len -8) s) ; MSB
    (write-byte (logand len #xff) s) ; LSB
    (write-hufftable +luminance-dc-bits+ +luminance-dc-values+ 0 s)
    (write-hufftable +luminance-ac-bits+ +luminance-ac-values+ 16 s)
    (write-hufftable +chrominance-dc-bits+ +chrominance-dc-values+ 1 s)
    (write-hufftable +chrominance-ac-bits+ +chrominance-ac-values+ 17 s)))

;;; Writes single huffman table
(defun write-hufftable (bits vals tcti s)
  (declare (type fixnum tcti))
    (write-byte tcti s) ; Tc/Th
    (write-sequence bits s)
    (write-sequence vals s))

;;; Drops specified marker into the stream
(defun write-marker (code to)
   (write-byte #xFF to)
   (write-byte code to))

;;; Writing some markers into the stream
(defun prepare-JFIF-stream (out-stream)
   (write-marker +M_SOI+ out-stream)
   (write-marker +M_APP0+ out-stream)
   (write-byte 0 out-stream) ; length
   (write-byte 16 out-stream)
   (write-byte #x4a out-stream)
   (write-byte #x46 out-stream)
   (write-byte #x49 out-stream)
   (write-byte #x46 out-stream)
   (write-byte 0 out-stream)
   (write-byte 1 out-stream) ; version
   (write-byte 2 out-stream)
   (write-byte 0 out-stream) ; units
   (write-byte 0 out-stream) ; density
   (write-byte 1 out-stream)
   (write-byte 0 out-stream)
   (write-byte 1 out-stream)
   (write-byte 0 out-stream) ; thumbnail
   (write-byte 0 out-stream))

;;; Builds common decoding and encoding tables
(defun build-universal-tables (bits)
  (let ((huffsize (make-array 256))
        (huffcode (make-array 256))
        (lastk 0))
    (declare #.*optimize* (type fixnum lastk)
             (type (simple-vector *) bits huffcode huffsize))
      ;; generating huffsize
      (loop for i fixnum from 1 to 16
            with k fixnum = 0 and j fixnum = 1 do
            (loop until (> j (svref bits (1- i))) do
                  (setf (svref huffsize k) i)
                  (incf k)
                  (incf j)
                  finally (setf j 1))
            finally (progn (setf lastk k) (setf (svref huffsize lastk) 0)))

      ;; generating huffcode
      (loop with k fixnum = 0 and code fixnum = 0 and si fixnum = (svref huffsize 0) do
            (loop do
                  (setf (svref huffcode k) code)
                  (incf code)
                  (incf k)
                  when (/= (svref huffsize k) si) do (return))
            when (zerop (svref huffsize k)) do
            (return)
            else do
            (loop do
                  (setf code (ash code 1))
                  (incf si)
                  when (= (svref huffsize k) si) do (return)))
      (values huffcode huffsize lastk)))

;;;Builds ordered code tables for encoder
(defun build-tables (bits huffval)
  (let ((ehufco (make-array 256))
        (ehufsi (make-array 256)))
    (multiple-value-bind (huffcode huffsize lastk)
        (build-universal-tables bits)
      (declare (type (simple-vector *) huffsize huffcode)
               (type fixnum lastk))
      (loop with i fixnum for k from 0 below lastk do
            (setf i (svref huffval k))
            (setf (svref ehufco i) (svref huffcode k))
            (setf (svref ehufsi i) (svref huffsize k)))
      (list ehufsi ehufco))))

;;; Main encoder function (user interface)
(defun encode-image-stream (out-stream image ncomp h w
                            &key (q-tabs +q-tables+) (sampling '((2 2)(1 1)(1 1))) (q-factor 64))
  (declare #.*optimize*
           (type fixnum ncomp h w q-factor)
           (type (simple-array uint8 (*)) image))
  (when (= ncomp 1)
    (setq sampling '((1 1))))
  (let* ((wd (loop for entry in sampling maximize (first entry)))
         (ht (loop for entry in sampling maximize (second entry)))
	 (*zz-result* (make-array 64 :element-type 'unsigned-byte))
	 (*prev-byte* 0) ; State variables for write-bits
	 (*prev-length* 0)
         (isampling (convert-sampling sampling wd ht))
         (height (ash ht 3))
         (width (ash wd 3))
         (YUV (make-array ncomp
                          :initial-contents
                          (loop for i fixnum from 0 below ncomp collecting
                               (make-array height
                                           :initial-contents
                                           (loop for j fixnum from 0 below height
                                              collecting (make-array width))))))
         (sampled-buf (make-array (mul ht wd)
                                  :initial-contents
                                  (loop for b fixnum from 0 below (mul ht wd)
                                     collecting (make-array 8
                                                            :initial-contents
                                                            (loop for i fixnum from 0 to 7
                                                               collecting (make-array 8))))))
         (preds (make-array ncomp :initial-element 0))
         (tqv (case ncomp
                (3 #(0 1 1)) ; q-tables destinations for various component numbers
                (1 #(0))
                (2 #(0 1))
                (4 #(0 1 2 3))
                (otherwise (error "Illegal number of components specified")))))
    (declare (special *zz-result* *prev-byte* *prev-length*)
	     (type fixnum *prev-length* *prev-byte*))
    (cond ((/= ncomp (length sampling))
           (error "Wrong sampling list for ~D component(s)" ncomp))
          ((> (length q-tabs) ncomp)
           (error "Too many quantization tables specified"))
          ((zerop q-factor)
           (error "Q-factor should be nonzero!"))
          ((> (count-relation sampling) 10)
           (error "Invalid sampling specification!")))
    (when (< q-factor 64)
      (let ((q-tabs2 (make-array (length q-tabs)
                                 :initial-contents
                                 (loop for k fixnum from 0 below (length q-tabs)
                                    collecting (make-array 8 :initial-contents
                                                           (loop for i fixnum from 0 to 7
                                                              collecting (make-array 8 :element-type 'uint8)))))))
        (loop for entry across q-tabs
           for entry2 across q-tabs2 do
           (loop for x fixnum from 0 to 7 do
                (loop for y fixnum from 0 to 7 do
                     (setf (dbref entry2 x y) (the fixnum (dbref entry x y))))))
        (setq q-tabs q-tabs2))
      (loop for entry across q-tabs do  ; scaling all q-tables
           (q-scale entry q-factor)))
    (setq *prev-byte* 0)
    (setq *prev-length* 0)
    (if (and (/= ncomp 1) (/= ncomp 3))
        (write-marker +M_SOI+ out-stream)
        (prepare-JFIF-stream out-stream))
    (write-frame-header w h ncomp q-tabs sampling tqv out-stream) ; frame header
    ;; writing scan header
    (write-marker +M_SOS+ out-stream)
    (write-byte 0 out-stream)           ; length
    (write-byte (plus 6 (ash ncomp 1)) out-stream)
    (write-byte ncomp out-stream)    ; number of components in the scan
    (loop for Cj from 0 below ncomp do
         (write-byte Cj out-stream)     ; component ID
         (write-byte (if (zerop Cj) 0 17) out-stream)) ; TdTa
    (write-byte 0 out-stream)                          ; Ss
    (write-byte 63 out-stream)                         ; Se
    (write-byte 0 out-stream)                          ; AhAl

    (let ((luminance-tabset (list
                             (build-tables +luminance-dc-bits+ +luminance-dc-values+)
                             (build-tables +luminance-ac-bits+ +luminance-ac-values+)))
          (chrominance-tabset (list (build-tables +chrominance-dc-bits+ +chrominance-dc-values+)
                                    (build-tables +chrominance-ac-bits+ +chrominance-ac-values+))))
      (loop for dy fixnum from 0 below h by height do
           (loop for dx fixnum from 0 below w by width do
                (multiple-value-bind (xlim ylim)
                    (if (= ncomp 3)
                        (colorspace-convert image YUV dx dy h w height width)
                        (crop-image image YUV dx dy h w height width ncomp))
                  (declare (type fixnum xlim ylim)
                           (dynamic-extent xlim ylim))
                  (loop for comp across YUV
                     for freq in sampling
                     for ifreq across isampling
                     for iH fixnum = (first ifreq)
                     for iV fixnum = (second ifreq)
                     for cn fixnum from 0
                     for hufftabs = (if (zerop cn)
                                        luminance-tabset
                                        chrominance-tabset)
                     ;; choosing appropriate q-table for a component
                     for q-tab = (svref q-tabs (svref tqv cn))
                     for H fixnum = (first freq)
                     for V fixnum = (second freq) do
                     (subsample comp sampled-buf H V (minus xlim dx) (minus ylim dy) iH iV)
                     (loop for y fixnum from 0 below V
                        for ypos fixnum = (if (> (plus dy (ash y 3)) ylim)
                                              (mul (rem (ash ylim -3) V) H)
                                              (mul y H)) do
                        (loop for x fixnum from 0 below H
                           for pos fixnum = (if (> (plus dx (ash x 3)) xlim)
                                                (plus (rem (ash xlim -3) H) ypos)
                                                (plus x ypos)) do
                           (crunch sampled-buf pos q-tab)
                           (setf (svref preds cn)
                                 (encode-block (zigzag (svref sampled-buf pos))
                                               hufftabs (svref preds cn) out-stream)))))))))
    (unless (zerop *prev-length*)
      (write-stuffed (deposit-field #xff ; byte padding & flushing
                                    (byte (minus 8 *prev-length*) 0)
                                    (ash *prev-byte* (minus 8 *prev-length*)))
                     out-stream))
    (write-marker +M_EOI+ out-stream)))

(defun encode-image (filename image ncomp h w &rest args)
  (with-open-file (out-stream filename
                              :direction :output
                              :element-type 'unsigned-byte
                              :if-exists :supersede)
    (apply #'encode-image-stream out-stream image ncomp h w args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoder part

;;; Contains all information about a single scan
(defstruct scan
  (ncomp 1 :type fixnum) ; number of image components in the scan
  (x 0 :type fixnum) ; current processing
  (y 0 :type fixnum) ; positions
  (cdesc (make-array 4
                     :initial-contents (loop repeat 4 collect (list 0 0)))
         :type (simple-vector *))) ; descriptors of all components in the scan

;;; Contains huffman decoding tables
(defstruct huffstruct
  mincode
  maxcode
  (bits (make-array 16) :type (simple-vector *))
  huffval
  huffcode
  valptr)

;;; This structure contains all neccessary information about the decoded image
(defstruct descriptor
  (restart-interval 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  buffer
  (qtables
   (make-array 4 :initial-contents
               (loop for j fixnum from 0 to 3
                     collecting
                        (make-array 8 :initial-contents
                                    (loop for i fixnum from 0 to 7
                                          collecting (make-array 8))))) :type (simple-vector *))
  (huff-ac (make-array 2 :initial-contents
                       (list (make-huffstruct) (make-huffstruct))) :type (simple-vector *))
  (huff-dc (make-array 2 :initial-contents
                       (list (make-huffstruct) (make-huffstruct))) :type (simple-vector *))
  (cid (make-array 4) :type (simple-vector *))
  (scans (make-array 4 :initial-contents
                     (loop for i fixnum from 0 to 3 collecting (make-scan))) :type (simple-vector *))
  (H (make-array 4) :type (simple-vector *))
  (V (make-array 4) :type (simple-vector *))
  (iH (make-array 4) :type (simple-vector *))
  (iV (make-array 4) :type (simple-vector *))
  (qdest (make-array 4) :type (simple-vector *))
  (zz (make-array 64) :type (simple-vector *))
  (ncomp 0 :type fixnum))

;;; Reads an JPEG marker from the stream
(defun read-marker (s)
  (loop for b fixnum = (read-byte s)
        when (/= b #xff) do (return b)))

;;; Reads 16-bit word from the stream
(defun read-word (s)
  "Reads 16-bit word from the stream"
  (let* ((msb (ash (read-byte s) 8))
         (lsb (read-byte s))
         (word (logior msb lsb)))
    word))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; APPn marker reading: just skipping the whole marker
  (defun read-app (s)
    "APPn marker reading: just skipping the whole marker"
    (loop for i fixnum from 0 below (minus (read-word s) 2) do
         (read-byte s)))

;;; COM marker reading, same as read-app
  (setf (symbol-function 'read-com) #'read-app))

;;; Sets up restart interval
(defun read-dri (image s)
  "Reads restart interval"
  (read-byte s) ; skipping length
  (read-byte s)
  (setf (descriptor-restart-interval image) (read-word s)))

;;; 'Inverse zigzag transform'
(defun izigzag (inbuf zzbuf)
  (declare #.*optimize*
           (type (simple-vector *) inbuf zzbuf))
  "Performs inverse zigzag block arrangement"
  (loop for zrow across +zigzag-index+
        for row across zzbuf do
        (loop for pos fixnum across zrow
              for x fixnum from 0 do
              (setf (svref row x) (svref inbuf pos))))
  zzbuf)

;;; Reads in quantization tables
(defun read-dqt (image s)
  "Reads in quantization tables"
  (let ((len (minus (read-word s) 2)))
    (loop for i fixnum from (1- len) downto 0 by 65
          for tq fixnum = (logand (read-byte s) 7)
          with intable = (make-array 64)
          for table = (svref (descriptor-qtables image) tq) do
          (loop for pos from 0 to 63 do
                (setf (svref intable pos) (read-byte s)))
          (izigzag intable table))))

;;; Builds up decoder tables
(defun build-decoder-tables (bits huffcode)
  "Builds up decoder-specific tables"
  (let ((maxcode (make-array 17))
        (mincode (make-array 17))
        (valptr (make-array 17)))
    (loop with i fixnum = 0
          and j fixnum = 0 do
          (when (loop
                 (incf i)
                 (when (> i 16)
                   (return t))
                 (cond ((zerop (svref bits (1- i)))
                        (setf (svref maxcode i) -1))
                       (t (return nil))))
            (return (values maxcode mincode valptr)))
          (setf (svref valptr i) j)
          (setf (svref mincode i) (svref huffcode j))
          (incf j (1- (svref bits (1- i))))
          (setf (svref maxcode i) (svref huffcode j))
          (incf j))))

;;; Loads huffman tables
(defun read-dht (image s)
  "Loads huffman tables on specified destinations"
  (let ((len (minus (read-word s) 2))
        (count 0))
    (loop for tcth fixnum = (read-byte s)
          for tc fixnum = (ash tcth -4)
          for th fixnum = (logand tcth 15)
          for tables = (if (zerop tc)
                           (svref (descriptor-huff-ac image) th)
                         (svref (descriptor-huff-dc image) th))
          for bits = (huffstruct-bits tables)
          for sum fixnum = 0 do
          (loop for i fixnum from 0 to 15
                for entry fixnum = (read-byte s) do
                (incf sum entry)
                (setf (svref bits i) entry))
          (setf (huffstruct-huffval tables)
                (make-array sum :initial-contents (loop for i fixnum from 0 below sum
                                                        collecting (read-byte s))))
          (incf count (plus sum 17))
          (multiple-value-bind (maxcode mincode valptr)
              (build-decoder-tables bits (setf (huffstruct-huffcode tables)
                                               (build-universal-tables bits )))
            (declare (type (simple-vector *) maxcode mincode valptr))
            (setf (huffstruct-maxcode tables) maxcode)
            (setf (huffstruct-mincode tables) mincode)
            (setf (huffstruct-valptr tables) valptr))
          (unless (< count len) (return t)))))

;;; Reads tables etc., returns the first unrecognized marker it met
(defun interpret-markers (image term s)
  "Reads tables etc., returns the first unrecognized marker it met"
  (loop for mk fixnum = (cond ((zerop term) (read-marker s))
                              (t term)) do
        (setf term 0)
        (cond ((= #xe0 (logand #xf0 mk)) ; APPn marker
               (read-app s))
              (t (cond ((= mk +M_DAC+) (error "Arithmetic encoding not supported"))
                       ((= mk +M_DRI+) (read-dri image s))
                       ((= mk +M_DHT+) (read-dht image s))
                       ((= mk +M_DQT+) (read-dqt image s))
                       ((= mk +M_COM+) (read-com s))
                       (t (return mk)))))))

;;; EXTEND procedure, as described in the standard
(defun extend (v tt)
  "EXTEND procedure, as described in spec."
  (let ((vt (ash 2 (minus tt 2))))
    (if (< v vt)
        (plus v (1+ (ash -1 tt)))
      v)))

;;; Returns the closure which reads specified numbers of bits from the stream
(defun make-nextbit (b cnt)
  "Returns the closure which reads specified numbers of bits from the stream"
  #'(lambda (s)
      (let ((bit 0))
        (declare #.*optimize*
                 (type fixnum b cnt bit)
                 (type stream s))
        (when (zerop cnt)
          (setf b (read-byte s))
          (setf cnt 8)
          (when (= b #xff)
            (let ((b2 (read-byte s)))
              (declare (type fixnum b2))
              (cond ((zerop b2))
                    ((<= +M_RST0+ b2 +M_RST7+)
                     (throw 'marker 'restart))
                    ((= b2 +M_DNL+)
                     (error "DNL marker is not supported"))
                    (t (throw 'marker b2))))))
        (decf cnt)
        (setf bit (ash (logand b 255) -7))
        (setf b (ash b 1))
        bit)))

;;; The DECODE procedure
(defun decode (maxcode mincode valptr huffval nextbit s)
  "The DECODE procedure, as described in CCITT rec."
  (let ((i 1)
        (code (funcall nextbit s)))
    (loop while (> code (svref maxcode i)) do
          (incf i)
          (setf code (plus (ash code 1) (funcall nextbit s))))
    (svref huffval (plus (svref valptr i) (minus code (svref mincode i))))))

;;; Recieves ssss bits from the stream
(defun recieve (ssss nextbit s)
  "Recieves ssss bits from the stream"
  (let ((v 0))
    (declare #.*optimize*
             (type fixnum v ssss))
    (loop for i fixnum from 0
          until (= i ssss) do
          (setf v (plus (ash v 1) (funcall nextbit s))))
    v))

;;; Decodes AC coefficients
(defun decode-ac (zz maxcode mincode valptr huffval nextbit s)
  "Decodes AC coefficients"
  (declare #.*optimize*
           (type (simple-vector *) zz maxcode mincode valptr huffval))
  (fill zz 0 :start 1)
  (loop with k fixnum = 1
        for rs fixnum = (decode maxcode mincode valptr huffval nextbit s)
        for ssss fixnum = (logand rs 15)
        for r fixnum = (ash rs -4) do
        (cond ((zerop ssss)
               (if (= r 15)
                   (incf k 16)
                 (return zz)))
              (t (incf k r)
                 (setf (svref zz k)
                       (extend (recieve ssss nextbit s) ssss))
                 (if (= k 63)
                     (return zz)
                   (incf k))))))

;;; Decodes DC value
(defun decode-dc (maxcode mincode valptr huffval nextbit s)
  "Decodes DC value"
  (let ((tt (decode maxcode mincode valptr huffval nextbit s)))
  (declare #.*optimize*
           (type (simple-vector *)  maxcode mincode valptr huffval)
           (fixnum tt))
    (extend (recieve tt nextbit s) tt)))

;;; Decodes single 8x8 block
(defun decode-block (zz tabs nextbit s)
  "Reads one 8x8 block. Doesn't deals with predictors."
  (let ((tdc (svref tabs 0))
        (tac (svref tabs 1)))
  (declare #.*optimize*
           (type (simple-vector *) zz tabs)
           (type huffstruct tac tdc))
  (setf (svref zz 0) (decode-dc (huffstruct-maxcode tdc)
                                (huffstruct-mincode tdc)
                                (huffstruct-valptr tdc)
                                (huffstruct-huffval tdc) nextbit s))
  (decode-ac zz
             (huffstruct-maxcode tac)
             (huffstruct-mincode tac)
             (huffstruct-valptr tac)
             (huffstruct-huffval tac) nextbit s)
  zz))

;;; Dequanitzation
(defun dequantize (x y block table)
  "Dequantizes a single sample"
  (declare #.*optimize*
           (type fixnum x y)
           (type (simple-vector *) block table))
  (mul (dbref block x y) (dbref table x y)))

;;;Macro that bounds value in IDCT
(defmacro dct-limit (n)
  `(aref *idct-limit-array* (logand (plus ,n 255) 511)))

;;; Inverse LLM DCT and dequantization
(defun inverse-llm-dct (block q-table)
  "Performs Inverse LMM DCT and dequantizetion"
  (let ((tmp0 0) (tmp1 0) (tmp2 0) (tmp3 0)
        (tmp10 0) (tmp11 0) (tmp12 0) (tmp13 0)
        (z1 0) (z2 0) (z3 0) (z4 0) (z5 0)
        (dcval 0))
    (declare #.*optimize*
             (type fixnum tmp0 tmp1 tmp2 tmp3 tmp10 tmp11 tmp12 tmp13 z1 z2 z3 z4 z5 dcval)
             (type (simple-vector *) block)
             (dynamic-extent tmp0 tmp1 tmp2 tmp3 tmp10 tmp11 tmp12 tmp13 z1 z2 z3 z4 z5 dcval))
    (loop for dptr fixnum from 0 to 7 ; iterating over columns
          if (and (zerop (dbref block dptr 1))
                  (zerop (dbref block dptr 2))
                  (zerop (dbref block dptr 3))
                  (zerop (dbref block dptr 4))
                  (zerop (dbref block dptr 5))
                  (zerop (dbref block dptr 6))
                  (zerop (dbref block dptr 7))) do
          (setf dcval (ash (dequantize dptr 0 block q-table) 1))
          (setf (dbref *ws* dptr 0) dcval)
          (setf (dbref *ws* dptr 1) dcval)
          (setf (dbref *ws* dptr 2) dcval)
          (setf (dbref *ws* dptr 3) dcval)
          (setf (dbref *ws* dptr 4) dcval)
          (setf (dbref *ws* dptr 5) dcval)
          (setf (dbref *ws* dptr 6) dcval)
          (setf (dbref *ws* dptr 7) dcval)
          else do
          (setf z2 (dequantize dptr 2 block q-table))
          (setf z3 (dequantize dptr 6 block q-table))
          (setf z1 (mul (plus z2 z3) +FIX-0-541196100+))
          (setf tmp2 (plus z1 (mul z3 (- +FIX-1-847759065+))))
          (setf tmp3 (plus z1 (mul z2 +FIX-0-765366865+)))
          (setf z2 (dequantize dptr 0 block q-table))
          (setf z3 (dequantize dptr 4 block q-table))
          (setf tmp0 (ash (plus z2 z3) dct-shift))
          (setf tmp1 (ash (minus z2 z3) dct-shift))
          (setf tmp10 (plus tmp0 tmp3))
          (setf tmp13 (minus tmp0 tmp3))
          (setf tmp11 (plus tmp1 tmp2))
          (setf tmp12 (minus tmp1 tmp2))
          (setf tmp0 (dequantize dptr 7 block q-table))
          (setf tmp1 (dequantize dptr 5 block q-table))
          (setf tmp2 (dequantize dptr 3 block q-table))
          (setf tmp3 (dequantize dptr 1 block q-table))
          (setf z1 (plus tmp0 tmp3))
          (setf z2 (plus tmp1 tmp2))
          (setf z3 (plus tmp0 tmp2))
          (setf z4 (plus tmp1 tmp3))
          (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
          (setf tmp0 (mul tmp0 +FIX-0-298631336+))
          (setf tmp1 (mul tmp1 +FIX-2-053119869+))
          (setf tmp2 (mul tmp2 +FIX-3-072711026+))
          (setf tmp3 (mul tmp3 +FIX-1-501321110+))
          (setf z1 (mul z1 (- +FIX-0-899976223+)))
          (setf z2 (mul z2 (- +FIX-2-562915447+)))
          (setf z3 (mul z3 (- +FIX-1-961570560+)))
          (setf z4 (mul z4 (- +FIX-0-390180644+)))
          (incf z3 z5)
          (incf z4 z5)
          (incf tmp0 (plus z1 z3))
          (incf tmp1 (plus z2 z4))
          (incf tmp2 (plus z2 z3))
          (incf tmp3 (plus z1 z4))
          (setf (dbref *ws* dptr 0) (descale (plus tmp10 tmp3) +shift-1+))
          (setf (dbref *ws* dptr 7) (descale (minus tmp10 tmp3) +shift-1+))
          (setf (dbref *ws* dptr 1) (descale (plus tmp11 tmp2) +shift-1+))
          (setf (dbref *ws* dptr 6) (descale (minus tmp11 tmp2) +shift-1+))
          (setf (dbref *ws* dptr 2) (descale (plus tmp12 tmp1) +shift-1+))
          (setf (dbref *ws* dptr 5) (descale (minus tmp12 tmp1) +shift-1+))
          (setf (dbref *ws* dptr 3) (descale (plus tmp13 tmp0) +shift-1+))
          (setf (dbref *ws* dptr 4) (descale (minus tmp13 tmp0) +shift-1+)))

    (loop for row across block ; iterating over rows
          for inrow across *ws*
          if (not (find-if-not #'zerop inrow :start 1)) do
          (setf dcval (dct-limit (descale (svref inrow 0) 4)))
          (setf (svref row 0) dcval)
          (setf (svref row 1) dcval)
          (setf (svref row 2) dcval)
          (setf (svref row 3) dcval)
          (setf (svref row 4) dcval)
          (setf (svref row 5) dcval)
          (setf (svref row 6) dcval)
          (setf (svref row 7) dcval)
          else do
          (setf z2 (svref inrow 2))
          (setf z3 (svref inrow 6))
          (setf z1 (mul (plus z2 z3) +FIX-0-541196100+))
          (setf tmp2 (plus z1 (mul z3 (- +FIX-1-847759065+))))
          (setf tmp3 (plus z1 (mul z2 +FIX-0-765366865+)))
          (setf tmp0 (ash (plus (svref inrow 0) (svref inrow 4)) dct-shift))
          (setf tmp1 (ash (minus (svref inrow 0) (svref inrow 4)) dct-shift))
          (setf tmp10 (plus tmp0 tmp3))
          (setf tmp13 (minus tmp0 tmp3))
          (setf tmp11 (plus tmp1 tmp2))
          (setf tmp12 (minus tmp1 tmp2))
          (setf tmp0 (svref inrow 7))
          (setf tmp1 (svref inrow 5))
          (setf tmp2 (svref inrow 3))
          (setf tmp3 (svref inrow 1))
          (setf z1 (plus tmp0 tmp3))
          (setf z2 (plus tmp1 tmp2))
          (setf z3 (plus tmp0 tmp2))
          (setf z4 (plus tmp1 tmp3))
          (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
          (setf tmp0 (mul tmp0 +FIX-0-298631336+))
          (setf tmp1 (mul tmp1 +FIX-2-053119869+))
          (setf tmp2 (mul tmp2 +FIX-3-072711026+))
          (setf tmp3 (mul tmp3 +FIX-1-501321110+))
          (setf z1 (mul z1 (- +FIX-0-899976223+)))
          (setf z2 (mul z2 (- +FIX-2-562915447+)))
          (setf z3 (mul z3 (- +FIX-1-961570560+)))
          (setf z4 (mul z4 (- +FIX-0-390180644+)))
          (incf z3 z5)
          (incf z4 z5)
          (incf tmp0 (plus z1 z3))
          (incf tmp1 (plus z2 z4))
          (incf tmp2 (plus z2 z3))
          (incf tmp3 (plus z1 z4))
          (setf (svref row 0) (dct-limit (descale (plus tmp10 tmp3) +shift+4+)))
          (setf (svref row 7) (dct-limit (descale (minus tmp10 tmp3) +shift+4+)))
          (setf (svref row 1) (dct-limit (descale (plus tmp11 tmp2) +shift+4+)))
          (setf (svref row 6) (dct-limit (descale (minus tmp11 tmp2) +shift+4+)))
          (setf (svref row 2) (dct-limit (descale (plus tmp12 tmp1) +shift+4+)))
          (setf (svref row 5) (dct-limit (descale (minus tmp12 tmp1) +shift+4+)))
          (setf (svref row 3) (dct-limit (descale (plus tmp13 tmp0) +shift+4+)))
          (setf (svref row 4) (dct-limit (descale (minus tmp13 tmp0) +shift+4+))))))

;;; Places decoded block into the image buffer, with necessary upsampling
(defun upsample (image scan block x y H V offset nwidth nw nx dend)
  "Places decoded block into the image buffer, with necessary upsampling"
  (let* ((buffer (descriptor-buffer image))
         (ncomp (descriptor-ncomp image))
         (xbase (plus (scan-x scan) x)) ; (mul (ash x 3) H)))
         (ybase (plus (scan-y scan) y)) ; (mul (ash y 3) V)))
         (nxbase (mul xbase ncomp))
         (nybase (mul ybase nwidth)))
    (declare #.*optimize*
             (type (simple-vector *) block)
             (type (simple-array uint8 (*)) buffer)
             (type fixnum x y H V ncomp xbase ybase nwidth nx dend nxbase nybase offset)
             (dynamic-extent ncomp xbase ybase nxbase nybase))
    (loop for row across block
          for y fixnum from ybase below (descriptor-height image) by V
          for ypos fixnum from nybase by nw do
          (loop for val fixnum across row
                for x fixnum from xbase below (descriptor-width image) by H
                for pos fixnum from (+ ypos offset nxbase) by nx do
                (if (= 1 H V)
                    (setf (aref buffer pos) (the uint8 val))
                  (loop for dy fixnum from 0 below V
                        for dypos fixnum from pos below dend by nwidth
                        for dxend from (mul (plus (1+ y) dy) nwidth) by nwidth do
                        (loop for dx fixnum from 0 below H
                              for dpos fixnum from dypos below dxend by 3 do
                              (setf (aref buffer dpos) (the uint8 val)))))))))

;;; Reads and decodes either whole scan or restart interval
(defun decode-chunk (image scan s)
  "Reads and decodes either a whole scan (if no restarts) or restart interval"
  (let* ((nextbit (make-nextbit 0 0))
         (ncomp (scan-ncomp scan))
         (nwidth (mul (descriptor-width image) (descriptor-ncomp image)))
         (dend (mul (descriptor-height image) nwidth))
         (fr (make-array ncomp :initial-contents
                         (loop
                           ;; collecting sampling rates for a components in the scan
                           for i fixnum from 0 below ncomp
                           for cid fixnum = (first (svref (scan-cdesc scan) i))
                           for pos fixnum = (position cid (descriptor-cid image))
                           collecting (list (svref (descriptor-H image) pos)
                                            (svref (descriptor-V image) pos)))))
         (Hmax (loop for entry across fr maximize (first entry)))
         (Vmax (loop for entry across fr maximize (second entry)))
         (x-growth (ash Hmax 3))
         (y-growth (ash Vmax 3))
	 (*ws* (make-array 8 :initial-contents (loop for i from 0 to 7 collecting (make-array 8)))) ; Temporary workspace for IDCT
         (freqs (make-array ncomp :initial-contents
                            (loop for i fixnum from 0 below ncomp ; collecting sampling frequencies
                                  for cid fixnum = (first (svref (scan-cdesc scan) i))
                                  for pos fixnum = (position cid (descriptor-cid image))
                                  collecting (list (svref (descriptor-iH image) pos)
                                                   (svref (descriptor-iV image) pos)))))
         (preds (make-array ncomp :initial-element 0)))
    (declare #.*optimize*
             (type fixnum ncomp Hmax Vmax x-growth y-growth nwidth)
             (type (simple-vector *) freqs fr)
	     (special *ws*)
             (dynamic-extent fr freqs))
    (catch 'marker
      (loop
        with tables =
           (make-array ncomp
                       :initial-contents
                       (loop for i fixnum from 0 below ncomp
                             for ta fixnum = (logand (second (svref (scan-cdesc scan) i)) 15)
                             for td fixnum = (ash (second (svref (scan-cdesc scan) i)) -4)
                             collecting (vector (svref (descriptor-huff-ac image) ta)
                                                (svref (descriptor-huff-dc image) td))))
        do (loop for comp fixnum from 0 below ncomp
		 with zzbuf = #(#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0)
				#(0  0  0  0  0  0  0  0))
                 for pos fixnum =
                    (position (first (svref (scan-cdesc scan) comp))
                              (descriptor-cid image)) ; an offset for byte positioning
                 for q-tab = (svref (descriptor-qtables image) (svref (descriptor-qdest image) comp))
                 for H fixnum = (first (svref freqs comp))
                 for V fixnum = (second (svref freqs comp))
                 for nw fixnum = (mul nwidth V)
                 for nx fixnum = (mul (descriptor-ncomp image) H)
                 for blocks-y fixnum = (second (svref fr comp))
                 for blocks-x fixnum = (first (svref fr comp)) do
                    (loop for y fixnum from 0 below blocks-y
                          for y-pos fixnum from (mul (ash y 3) V) by (ash V 3) do
                             (loop for x fixnum from 0 below blocks-x
                                   for x-pos fixnum from (mul (ash x 3) H) by (ash H 3)
                                   for decoded-block =
                                      (izigzag (decode-block (descriptor-zz image)
                                                             (svref tables comp) nextbit s) zzbuf) do
                                   ;; DC decoding and predictor update
                                      (incf (dbref decoded-block 0 0) (svref preds comp))
                                      (setf (svref preds comp) (dbref decoded-block 0 0))
                                      (when (and (< (plus x-pos (scan-x scan)) (descriptor-width image))
                                                 (< (plus y-pos (scan-y scan)) (descriptor-height image)))
                                        ;; inverse DCT and block write to the buffer
                                        (inverse-llm-dct decoded-block q-tab)
                                        (upsample image scan decoded-block x-pos y-pos
                                                  H V pos nwidth nw nx dend)))))
           (incf (scan-x scan) x-growth)
           (when (<= (descriptor-width image) (scan-x scan))
             (incf (scan-y scan) y-growth)
             (setf (scan-x scan) 0))))))

;;; Scan decoding subroutine
(defun decode-scan (image i s)
  (let ((scan (svref (descriptor-scans image) i)))
    (read-byte s) ; length
    (read-byte s)
    (loop with ncomp fixnum = (setf (scan-ncomp scan) (read-byte s))
          for j fixnum from 0 below ncomp do
          (setf (first (svref (scan-cdesc scan) j)) (read-byte s)) ; component ID
          (setf (second (svref (scan-cdesc scan) j)) (read-byte s))) ; Td and Ta nibbles
    (read-byte s)
    (read-byte s)
    (read-byte s)
    (if (= (descriptor-restart-interval image) 0)
        (decode-chunk image scan s) ; reading the whole scan at once
      (loop for term = (decode-chunk image scan s)
            while (eq 'restart term)
            finally (return term))))) ; or in pieces

;;; Macro that bounds value in 0..255 range
(defmacro limit (n)
  `(cond ((> ,n 254) 255)
         ((< ,n 1) 0)
         (t ,n)))

;;; Inverse colorspace conversion
(defun inverse-colorspace-convert (image)
  (let* ((buffer (descriptor-buffer image))
         (nw (mul (descriptor-width image) 3)))
    (declare #.*optimize*
             (type (simple-array uint8 (*)) buffer)
             (type fixnum nw))
    (loop for y fixnum from 0 below (descriptor-height image)
          for yp fixnum from 0 by nw do
          (loop for x fixnum from 0 below (descriptor-width image)
                for py fixnum from yp by 3
                for pu fixnum = (1+ py)
                for pv fixnum = (plus py 2)
                for yy fixnum = (aref buffer py)
                for cb fixnum = (aref buffer pu)
                for cr fixnum = (aref buffer pv) do
                (setf (aref buffer py) ; BLUE
                      (the uint8 (limit (plus yy (svref *cb-b-tab* cb)))))
                (setf (aref buffer pu) ; GREEN
                      (the uint8 (limit (plus yy (ash (plus
						       (svref *cb-g-tab* cb)
						       (svref *cr-g-tab* cr))
						      (- shift))))))
                (setf (aref buffer pv) ; RED
                      (the uint8 (limit (plus yy (svref *cr-r-tab* cr)))))))))

(defun decode-frame-beginning (image s)
  (read-byte s) ; length
  (read-byte s)
  (read-byte s) ; sample precision
  (setf (descriptor-buffer image)
        (make-array (* (setf (descriptor-height image) (read-word s)) ; height
                       (setf (descriptor-width image) (read-word s))  ; width
                       (setf (descriptor-ncomp image) (read-byte s))) ; number of components
                    :element-type 'uint8
                    :initial-element 0)))

;;; Frame decoding subroutine
(defun decode-frame (image s)
  (decode-frame-beginning image s)
  (loop for i fixnum from 0 below (descriptor-ncomp image)
        with hv fixnum do
        (setf (svref (descriptor-cid image) i) (read-byte s)) ; Cj
        (setf hv (read-byte s)) ; HV
        (setf (svref (descriptor-H image) i) (ash hv -4))
        (setf (svref (descriptor-V image) i) (logand hv 7))
        (setf (svref (descriptor-qdest image) i) (read-byte s)))
  (let* ((frl (loop for i fixnum from 0 below (descriptor-ncomp image)
                    collecting (list (svref (descriptor-H image) i)
                                     (svref (descriptor-V image) i))))
         (Hmax (loop for entry in frl maximize (first entry)))
         (Vmax (loop for entry in frl maximize (second entry)))
         (freqs (convert-sampling frl Hmax Vmax)))
    (loop for entry across freqs
          for i fixnum from 0 do
          (setf (svref (descriptor-iH image) i) (first entry))
          (setf (svref (descriptor-iV image) i) (second entry)))
    (loop with term fixnum = 0
          for j fixnum from 0
          until (= term +M_EOI+) do
          (when (/= (interpret-markers image term s) +M_SOS+)
                (error "Unsupported marker in the frame header"))
          (setf term (decode-scan image j s)))
    (when (= (descriptor-ncomp image) 3)
      (inverse-colorspace-convert image))))

(defun decode-stream (stream)
  "Return image array, height, width, and number of components. Does not support
progressive DCT-based JPEGs."
  (unless (= (read-marker stream) +M_SOI+)
    (error "Unrecognized JPEG format"))
  (let* ((image (make-descriptor))
         (marker (interpret-markers image 0 stream)))
    (cond ((= +M_SOF0+ marker) (decode-frame image stream)
           (values (descriptor-buffer image)
                   (descriptor-height image)
                   (descriptor-width image)
                   (descriptor-ncomp image)))
          (t (error "Unsupported JPEG format: ~A" marker)))))

;;; Top level decoder function
(defun decode-image (filename)
  (with-open-file (in filename :direction :input :element-type 'unsigned-byte)
    (decode-stream in)))

(defun decode-stream-height-width (stream)
  "Return the height and width of the JPEG data read from STREAM. Does less work than
DECODE-STREAM and also supports progressive DCT-based JPEGs."
  (unless (= (read-marker stream) +M_SOI+)
    (error "Unrecognized JPEG format"))
  (let* ((image (make-descriptor)) ;; KLUDGE doing a lot of extra consing here
         (marker (interpret-markers image 0 stream)))
    (cond ((or (= +M_SOF0+ marker)
               (= +M_SOF2+ marker)) (decode-frame-beginning image stream)
           (values (descriptor-height image)
                   (descriptor-width image)))
          (t (error "Unsupported JPEG format: ~A" marker)))))

