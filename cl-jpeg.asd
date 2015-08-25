;;;; -*- Mode: Lisp; Package: User; -*-

(asdf:defsystem :cl-jpeg
  :name "cl-jpeg"
  :version "1.3"
  :license "BSD"
  :description "A self-contained baseline JPEG codec implementation"
  :author "Eugene Zaikonnikov; contributions from Cyrus Harmon and William Halliburton"
  :components ((:file "package")
	       (:file "jpeg" :depends-on ("package"))
	       (:file "io" :depends-on ("jpeg"))))
