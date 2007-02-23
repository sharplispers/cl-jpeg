;;;; -*- Mode: Lisp; Package: User; -*-

(defpackage #:cl-jpeg-system (:use #:asdf #:cl))
(in-package #:cl-jpeg-system)

(defsystem :cl-jpeg
  :components ((:file "jpeg")))

