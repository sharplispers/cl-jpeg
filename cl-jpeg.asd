(asdf:defsystem :cl-jpeg
  :name "cl-jpeg"
  :version "1.8"
  :license "BSD"
  :description "A self-contained baseline JPEG codec implementation"
  :author "Eugene Zaikonnikov; contributions by Manuel Giraud, Cyrus Harmon and William Halliburton"
  :components ((:file "package")
	       (:file "jpeg" :depends-on ("package"))
	       (:file "io" :depends-on ("jpeg"))))
